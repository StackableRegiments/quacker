package metl.comet

import metl.model._
import net.liftweb._
import net.liftweb.actor._
import net.liftweb.common._
import http._
import js._
import JsCmds._
import json.JsonDSL._
import json.JsonAST._
import util._
import Helpers._

import xml._
import java.util.Date

import net.liftweb.http.js.JE.Call

case class StatusCall(id:String,replacementActions:JsCmd, service:String, server:String, serviceCheckMode:ServiceCheckMode)

case class RemoveCheck(check:VisualElement)
case class CreateCheck(check:VisualElement)

object CheckAction extends Enumeration {
  type CheckAction = Value
  val Create,Destroy,Update,CreateBulk,DestroyBulk,UpdateBulk,History = Value
}

trait CheckRenderHelper {
  protected val checkActionJsCmds = Map(
    CheckAction.CreateBulk -> "createChecks",
    CheckAction.Create -> "createCheck",
    CheckAction.DestroyBulk -> "removeChecks",
    CheckAction.Destroy -> "removeCheck",
    CheckAction.UpdateBulk -> "updateChecks",
    CheckAction.Update -> "updateCheck"
  )
  protected val statusClasses = List("serverOk","serverError","serverUnknown")
  def jsCmdCreator(action:CheckAction.Value, c:CheckResult):JsCmd = Call(checkActionJsCmds(action),jsonForCheckResult(c))
  def jsCmdCreator(action:CheckAction.Value,v:VisualElement):JsCmd = Call(checkActionJsCmds(action),jsonForVisualElement(v))
  def jsonForCheckResult(s:CheckResult) = s.generateJson
  def jsonForVisualElement(v:VisualElement): JObject = v.jsonRenderer()
}

object DashboardServer extends LiftActor with ListenerManager{
  def createUpdate: NodeSeq = NodeSeq.Empty
  override def lowPriority: PartialFunction[Any, Unit] = {
    case r:RemoveCheck => sendListenersMessage(r)
    case c:CreateCheck => sendListenersMessage(c)
    case c:CheckResult => sendListenersMessage(c)
		case s:StatusCall	=> sendListenersMessage(s)
    case _ => {}
  } 
}

class Dashboard extends CometActor with CometListener with CheckRenderHelper {
  override def lifespan:Box[TimeSpan] = Full(5 minutes)
  def registerWith: DashboardServer.type = DashboardServer
  def getJsonStructure: List[JObject] = Servers.getVisualElements.map(ve => jsonForVisualElement(ve))
  override def render = {
    OnLoad(Call(checkActionJsCmds(CheckAction.CreateBulk),JArray(getJsonStructure)))
  }

  override def lowPriority: PartialFunction[Any, Unit] = {
    case s:CheckResult if Globals.currentUserAccessRestriction.permit(s) => partialUpdate(jsCmdCreator(CheckAction.Update,s))
    case CreateCheck(check) if Globals.currentUserAccessRestriction.permit(check)=> partialUpdate(Call(checkActionJsCmds(CheckAction.Create),jsonForVisualElement(check)))
    case RemoveCheck(check) if Globals.currentUserAccessRestriction.permit(check)=> partialUpdate(Call(checkActionJsCmds(CheckAction.Destroy),JString(check.id)))
    case _ => {}
  }
}
