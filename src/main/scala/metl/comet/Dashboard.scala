package metl.comet

import org.apache.commons.io.IOUtils
import metl.model._
import net.liftweb._
import net.liftweb.actor._
import net.liftweb.common._
import http._
import js._
import JsCmds._
import JE._
import js.jquery.JqJE._
import json.JsonDSL._
import json.JsonAST._
import json.JsonParser._
import util._
import Helpers._
import xml._
import net.liftweb.http.SHtml._
import java.util.Date
import scala.util.Random.shuffle
import collection.JavaConverters._
import net.liftweb.common.Logger
import net.liftweb.util.TimeHelpers

case class StatusCall(id:String,replacementActions:JsCmd, service:String, server:String, serviceCheckMode:ServiceCheckMode)

object CheckAction extends Enumeration {
  type CheckAction = Value
  val Create,Destroy,Update = Value
}

trait CheckRenderHelper {
  protected val checkActionJsCmds = Map(
    CheckAction.Create -> "createCheck",
    CheckAction.Destroy -> "destroyCheck",
    CheckAction.Update -> ""
  )
  protected val statusClasses = List("serverOk","serverError","serverUnknown")
  protected def statusClassFromStatus(status:Box[Boolean]):String = {
    status.map(s => s match {
      case true => "serverOk"
      case false => "serverError"
    }).openOr("serverUnknown")
  }
  protected def statusCodeFromStatus(status:Box[Boolean]):String = {
    status.map(s => s match {
      case true => "Y"
      case false => "N"
    }).openOr("?")
  }
  protected def uptimeString(date:Box[Date]):String = date.map(d => d.toString).openOr("NEVER")
  def jsCmdCreator(action:CheckAction.Value, c:CheckResult):JsCmd = Call(checkActionJsCmds(action),jsonForCheckResult(c))
  def jsCmdCreator(action:CheckAction.Value,v:VisualElement):JsCmd = Call(checkActionJsCmds(action),jsonForVisualElement(v))
  def jsonForCheckResult(s:CheckResult) = JObject(List(
    JField("id",JString(s.id)),
    JField("statusCode",JString(statusCodeFromStatus(Full(s.success)))),
    JField("statusClass",statusClasses.map(sc => {
      JField(sc, sc match {
        case str:String if (str == statusClassFromStatus(Full(s.success))) => JBool(true)
        case _ => JBool(false)
      })
    })),
    JField("label",JString(s.label)),
    JField("service",JString(s.service)),
    JField("server",JString(s.server)),
    JField("now",JString(now.toString)),
    JField("why",JString(s.why)),
    JField("detail",JString(s.success match {
      case true => ""
      case _ => s.detail
    })),
    JField("lastUp",JString(uptimeString(s.lastUp))),
    JField("mode",JString(s.mode.toString))
  ))
  def jsonForVisualElement(v:VisualElement) = v.jsonRenderer()
  def jsExpForVisualElement(v:VisualElement) = v.jsExpRenderer()
}

object DashboardServer extends LiftActor with ListenerManager{
  def createUpdate = NodeSeq.Empty
  override def lowPriority = {
    case c:CheckResult => updateListeners(c)
    case s:StatusCall   => updateListeners(s)
    case _ => {}
  }
}

class Dashboard extends CometActor with CometListener with CheckRenderHelper {
  override def lifespan:Box[TimeSpan] = Full(5 minutes)
  def registerWith = DashboardServer
  def getJsonStructure = Servers.getVisualElements.map(ve => jsonForVisualElement(ve))
  def getJsExpStructure = JsObj(Servers.getVisualElements.map(ve => (ve.id,jsExpForVisualElement(ve))):_*)
  override def render = NodeSeq.Empty
  override def fixedRender = {
    "#jsonStructureContainer *" #> Script(JsCrVar("jsonStructure",getJsExpStructure)) //&
                                                                                      //    "#serverContainer *" #> Servers.structure
  }

  override def lowPriority = {
    case s:CheckResult if Globals.currentUserAccessRestriction.permit(s) => partialUpdate(jsCmdCreator(CheckAction.Update,s))
    case _ => {}
  }
}
