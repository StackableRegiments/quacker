package metl.comet

import metl.model._
import net.liftweb._
import net.liftweb.actor._
import net.liftweb.common._
import http._
import js._
import JsCmds._
import JE._
import json._
import Serialization._
import util._
import Helpers._
import SHtml._

import xml._
import java.util.Date

import scala.concurrent.{Future, ExecutionContext}

case class StatusCall(id: String,
                      replacementActions: JsCmd,
                      service: String,
                      server: String,
                      serviceCheckMode: ServiceCheckMode)

case class UserLoggedIn(session: LiftSession)

case class RemoveCheck(check: VisualElement)
case class CreateCheck(check: VisualElement)

object CheckAction extends Enumeration {
  type CheckAction = Value
  val Create, Destroy, Update, CreateBulk, DestroyBulk, UpdateBulk, History,
  FetchBulk, AllHistory, CreateHistory = Value
}

trait CheckRenderHelper {
  protected val checkActionJsCmds = Map(
    CheckAction.FetchBulk -> "fetchChecks",
    CheckAction.AllHistory -> "fetchAllHistory",
    CheckAction.History -> "fetchHistory",
    CheckAction.CreateHistory -> "createHistoricalChecks",
    CheckAction.CreateBulk -> "createChecks",
    CheckAction.Create -> "createCheck",
    CheckAction.DestroyBulk -> "removeChecks",
    CheckAction.Destroy -> "removeCheck",
    CheckAction.UpdateBulk -> "updateChecks",
    CheckAction.Update -> "updateCheck"
  )
  protected val statusClasses = List("serverOk", "serverError", "serverUnknown")
  def jsCmdCreator(action: CheckAction.Value, c: CheckResult): JsCmd =
    Call(checkActionJsCmds(action), jsonForCheckResult(c))
  def jsCmdCreator(action: CheckAction.Value, v: VisualElement): JsCmd =
    Call(checkActionJsCmds(action), jsonForVisualElement(v))
  def jsonForCheckResult(s: CheckResult) = JObject(s.generateJson)
  def jsonForVisualElement(v: VisualElement): JObject = v.jsonRenderer()
}

object DashboardServer extends LiftActor with ListenerManager {
  def createUpdate: NodeSeq = NodeSeq.Empty
  override def lowPriority: PartialFunction[Any, Unit] = {
    case r: RemoveCheck  => sendListenersMessage(r)
    case c: CreateCheck  => sendListenersMessage(c)
    case c: CheckResult  => sendListenersMessage(c)
    case s: StatusCall   => sendListenersMessage(s)
    case u: UserLoggedIn => sendListenersMessage(u)
    case _               => {}
  }
}

class Dashboard extends CometActor with CometListener with CheckRenderHelper {
  implicit val ec = ExecutionContext.global
  implicit val formats = DefaultFormats
  override def lifespan: Box[TimeSpan] = Full(2 minutes)
  def registerWith: DashboardServer.type = DashboardServer
  def getJsonStructure: List[JObject] =
    Servers.getVisualElements.map(ve => jsonForVisualElement(ve))
  override def render = {
    Script(
      JsCrVar(
        checkActionJsCmds(CheckAction.FetchBulk),
        AnonFunc(
          ajaxCall(
            JsRaw("JSON.stringify(arguments)"),
            (s: String) => {
              Future({
                partialUpdate(Call(checkActionJsCmds(CheckAction.CreateBulk),
                                   JArray(getJsonStructure)))
              })
              Noop
            }
          ))
      ) &
        JsCrVar(
          checkActionJsCmds(CheckAction.History),
          AnonFunc(
            ajaxCall(
              JsRaw("JSON.stringify(arguments)"),
              (s: String) => {
                val args: JValue = parse(s)
                val parsedArgs = for {
                  service <- (args \ "0").extractOpt[String]
                  server <- (args \ "1").extractOpt[String]
                  serviceCheck <- (args \ "2").extractOpt[String]
                  since = (args \ "3").extractOpt[Long]
                  until = (args \ "4").extractOpt[Long]
                  limit = (args \ "5").extractOpt[Int]
                } yield {
                  (service, server, serviceCheck, since, until, limit)
                }
                parsedArgs
                  .map(a => {
                    Future({
                      val results = HistoryServer
                        .getHistory(None, a._1, a._2, a._3, a._4, a._5, a._6)
                        .filter(check =>
                          Globals.currentUserAccessRestriction.permit(check))
                        .map(check => jsonForCheckResult(check))
                      partialUpdate(
                        Call(checkActionJsCmds(CheckAction.CreateHistory),
                             JArray(results)))
                    })
                    Noop
                  })
                  .getOrElse(Alert(
                    "error in history call - arguments required"))
              }
            ))
        ) &
        JsCrVar(
          checkActionJsCmds(CheckAction.AllHistory),
          AnonFunc(
            ajaxCall(
              JsRaw("JSON.stringify(arguments)"),
              (s: String) => {
                val args: JValue = parse(s)
                val parsedArgs = Some({
                  val since = (args \ "0").extractOpt[Long]
                  val until = (args \ "1").extractOpt[Long]
                  val limit = (args \ "2").extractOpt[Int]
                  (since, until, limit)
                })
                parsedArgs
                  .map(a => {
                    Future({
                      val results = HistoryServer
                        .getAllHistory(a._1, a._2, a._3)
                        .filter(check =>
                          Globals.currentUserAccessRestriction.permit(check))
                        .map(check => jsonForCheckResult(check))
                      partialUpdate(
                        Call(checkActionJsCmds(CheckAction.CreateHistory),
                             JArray(results)))
                    })
                    Noop
                  })
                  .getOrElse(Alert(
                    "error in history call - arguments required"))
              }
            ))
        ) &
        OnLoad(Call(checkActionJsCmds(CheckAction.FetchBulk), JNull))
    )
  }

  override def lowPriority: PartialFunction[Any, Unit] = {
    case s: CheckResult if Globals.currentUserAccessRestriction.permit(s) =>
      partialUpdate(jsCmdCreator(CheckAction.Update, s))
    case UserLoggedIn(s: LiftSession) if S.session.exists(_ == s) =>
      S.redirectTo("/simple")
    case CreateCheck(check)
        if Globals.currentUserAccessRestriction.permit(check) =>
      partialUpdate(
        Call(checkActionJsCmds(CheckAction.Create),
             jsonForVisualElement(check)))
    case RemoveCheck(check)
        if Globals.currentUserAccessRestriction.permit(check) =>
      partialUpdate(
        Call(checkActionJsCmds(CheckAction.Destroy), JString(check.id)))
    case _ => {}
  }
}
