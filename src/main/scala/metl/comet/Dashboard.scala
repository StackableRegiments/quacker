package metl.comet

import metl.model._
import net.liftweb._
import net.liftweb.actor._
import net.liftweb.common._
import http._
import js._
import JsCmds._
import JE._
import json.JsonDSL._
import json.JsonAST._
import util._
import Helpers._
import xml._
import java.util.Date

case class StatusCall(id:String,replacementActions:JsCmd, service:String, server:String, serviceCheckMode:ServiceCheckMode)

object CheckAction extends Enumeration {
  type CheckAction = Value
  val Create,Destroy,Update,History = Value 
}

trait CheckRenderHelper {
  protected val checkActionJsCmds = Map(
    CheckAction.Create -> "createCheck",
    CheckAction.Destroy -> "destroyCheck",
    CheckAction.Update -> "updateCheck"
  )
  protected val statusClasses = List("serverOk","serverError","serverUnknown")
  protected def statusClassFromStatus(status:Box[Boolean]):String = {
    status.map {
      case true => "serverOk"
      case false => "serverError"
    }.openOr("serverUnknown")
  }
  protected def statusCodeFromStatus(status:Box[Boolean]):String = {
    status.map {
      case true => "Y"
      case false => "N"
    }.openOr("?")
  }
  protected def uptimeString(date:Box[Date]):String = date.map(d => d.toString).openOr("NEVER")
  def jsCmdCreator(action:CheckAction.Value, c:CheckResult):JsCmd = Call(checkActionJsCmds(action),jsonForCheckResult(c))
  def jsCmdCreator(action:CheckAction.Value,v:VisualElement):JsCmd = Call(checkActionJsCmds(action),jsonForVisualElement(v))
  def jsonForCheckResult(s:CheckResult) = JObject(List(
    JField("id",JString(s.id)),
    JField("statusCode",JString(statusCodeFromStatus(Full(s.success)))),
    JField("statusClass",statusClasses.map(sc => {
      JField(sc, sc match {
        case str:String if str == statusClassFromStatus(Full(s.success)) => JBool(true)
        case _ => JBool(false)
      })
    })),
    JField("label",JString(s.label)),
    JField("now",JString(now.toString)),
    JField("why",JString(s.why)),
    JField("detail",JString(s.success match {
      case true => ""
      case _ => s.detail
    })),
    JField("lastUp",JString(uptimeString(s.lastUp))),
    JField("mode",JString(s.mode.toString)),
    JField("data",JArray(s.data.map(tup => {
      val when = tup._1
      val data = tup._2
      JObject(List(
        JField("when",JInt(tup._1)),
        JField("values",JObject(tup._2.toList.map(dTup => {
          JField(dTup._1,dTup._2 match {
            case GraphableFloat(f) => JDouble(f)
            case GraphableDouble(d) => JDouble(d)
            case GraphableInt(i) => JInt(i)
            case GraphableLong(l) => JInt(l)
            case GraphableString(s) => JString(s)
            case GraphableBoolean(b) => JBool(b)
          })
        })))
      ))
    })))
  ))
  def jsonForVisualElement(v:VisualElement): JObject = v.jsonRenderer()
  def jsExpForVisualElement(v:VisualElement): JsObj = v.jsExpRenderer()
}

object DashboardServer extends LiftActor with ListenerManager{
  def createUpdate: NodeSeq = NodeSeq.Empty
  override def lowPriority: PartialFunction[Any, Unit] = {
    case c:CheckResult => sendListenersMessage(c)
		case s:StatusCall	=> sendListenersMessage(s)
    case _ => {}
  } 
}

class Dashboard extends CometActor with CometListener with CheckRenderHelper {
  override def lifespan:Box[TimeSpan] = Full(5 minutes)
  def registerWith: DashboardServer.type = DashboardServer
  def getJsonStructure: List[JObject] = Servers.getVisualElements.map(ve => jsonForVisualElement(ve))
  def getJsExpStructure: JsObj = JsObj(Servers.getVisualElements.map(ve => (ve.id,jsExpForVisualElement(ve))):_*)
  override def render: RenderOut = NodeSeq.Empty
  override def fixedRender: Box[NodeSeq] = {
    "#jsonStructureContainer *" #> Script(JsCrVar("jsonStructure",getJsExpStructure)) 
  }

  override def lowPriority: PartialFunction[Any, Unit] = {
    case s:CheckResult if Globals.currentUserAccessRestriction.permit(s) => partialUpdate(jsCmdCreator(CheckAction.Update,s))
    case _ => {}
  }
}
