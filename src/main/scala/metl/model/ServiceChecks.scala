package metl.model

import metl.comet._
import net.liftweb._
import net.liftweb.actor._
import net.liftweb.common._
import http._
import js._
import JE._
import json.JsonAST._
import util.{Helpers, _}
import Helpers._

import xml._
import java.util.Date

import net.liftweb.common.Logger

case class DashboardException(reason:String,detail:String,exceptions:List[Exception] = Nil) extends Exception(reason)
case object Check
case object StartPinger
case object StopPinger

object ViewTemplates {
	//change these to vals to speed up the process
	def getStructureTemplate: NodeSeq = Templates(List("_Service")).openOr(NodeSeq.Empty)
	def getServiceTemplate: NodeSeq = Templates(List("_Check")).openOr(NodeSeq.Empty)
	def getInformationTemplate: NodeSeq = Templates(List("_Information")).openOr(NodeSeq.Empty)
	def getErrorInformationTemplate: NodeSeq = Templates(List("_ErrorInformation")).openOr(NodeSeq.Empty)
	def getEndpointInformationTemplate: NodeSeq = Templates(List("_EndpointInformation")).openOr(NodeSeq.Empty)
}

trait VisualElement {
	val id:String = nextFuncName
	val label:String
	protected var internalServiceName = "unknown"
	protected var internalServerName = "unknown"
	protected def template: NodeSeq = NodeSeq.Empty
	def classDescriptor(server:String,service:String):String = List("check",service,server,"toggleable").mkString(" ")
	def getServerName:String = internalServerName
	def serverName(newName:String):VisualElement = {
		internalServerName = newName
		this
	}
	def getServiceName:String = internalServiceName
	def serviceName(newName:String):VisualElement = {
		internalServiceName = newName
		this
	}
	def renderVisualElement(service:String = internalServiceName,server:String = internalServerName):NodeSeq = (
		".serviceCheck [id]" #> id &
		".serviceCheck [class+]" #> classDescriptor(server,service) &
		".serviceCapacity *" #> label &
		templateRenderer
	).apply(template).toSeq
	protected def templateRenderer:CssSel = ClearClearable
  def jsonRenderer(service:String = internalServiceName,server:String = internalServerName):JObject = {
    JObject(List(
      JField("id",JString(id)),
      JField("server",JString(server)),
      JField("service",JString(service)),
      JField("label",JString(label))
    ) ::: asJson)
  }
  protected def asJson:List[JField] = Nil
  def jsExpRenderer(service:String = internalServiceName,server:String = internalServerName):JsObj = {
    JsObj((List(
      ("id", Str(id)),
      ("server",Str(server)),
      ("service",Str(service)),
      ("label",Str(label))
    ) ::: asJsExp):_*)
  }
  protected def asJsExp:List[Tuple2[String,JsExp]] = Nil
}

case class HtmlInformation(name:String,html:NodeSeq) extends VisualElement {
	override val label: String = "%s information".format(name)
	override def template: NodeSeq = ViewTemplates.getInformationTemplate
	override def templateRenderer: CssBindFunc = {
		".serviceStatus [title]" #> label &
		".informationBody *" #> html
	}
  override def asJson = List(
    JField("type",JString("htmlInformation")),
    JField("html",JString(html.toString))
  )
  override def asJsExp = List(
    ("type",Str("htmlInformation")),
    ("html", Str(html.toString))
  )
}

case class Information(name:String,message:String) extends VisualElement {
	override val label: String = name
	override def template: NodeSeq = ViewTemplates.getInformationTemplate
	override def templateRenderer: CssBindFunc = {
		".serviceStatus [title]" #> label &
		".informationBody *" #> message
	}
  override def asJson = List(
    JField("type",JString("information")),
    JField("information",JString(message))
  )
  override def asJsExp = List(
    ("type",Str("information")),
    ("information",Str(message))
  )
}

case class ErrorInformation(name:String,expectedPeriod:String,sourceString:String,errors:List[String]) extends VisualElement {
	override val label: String = "Error: %s".format(name)
	override def template: NodeSeq = ViewTemplates.getErrorInformationTemplate
	override def templateRenderer: CssBindFunc = {
		".expectedPeriod *" #> expectedPeriod &
		".sourceString *" #> sourceString &
		".serviceStatus [title]" #> label &
		".errorList *" #> errors.map(e => {
			<span class="errorItem">{e}</span>
		})
	}
  override def asJson = List(
    JField("type",JString("error")),
    JField("source",JString(sourceString)),
    JField("expectedPeriod",JString(expectedPeriod)),
    JField("errors",JArray(errors.map(e => JString(e))))
  )
  override def asJsExp = List(
    ("type",Str("error")),
    ("source",Str(sourceString)),
    ("expectedPeriod",Str(expectedPeriod)),
    ("errors",JsArray(errors.map(e => Str(e))))
  )
}

case class EndpointInformationWithString(name:String,htmlDescriptor:String,endpoints:List[EndpointDescriptor]) extends EndpointInformationWithHtml(name,Text(htmlDescriptor),endpoints)
class EndpointInformationWithHtml(name:String,html:NodeSeq,endpoints:List[EndpointDescriptor]) extends VisualElement {
	override val label: String = name
	override def template: NodeSeq = ViewTemplates.getEndpointInformationTemplate
	override def templateRenderer: CssBindFunc = {
		".informationBody *" #> html &
		".serviceStatus [title]" #> label &
		".servicesList *" #> endpoints.map(ep => {
			".endpointName *" #> <a href={ep.endpoint}>{ep.name}</a> &
			".endpointName [title]" #> ep.description
		})
	}
  override def asJson = List(
    JField("type",JString("endpoints")),
    JField("information",JString(html.toString)),
    JField("endpoints",JArray(endpoints.map(ep => JObject(List(
      JField("url",JString(ep.endpoint)),
      JField("name",JString(ep.name)),
      JField("description",JString(ep.description))
    )))))
  )
  override def asJsExp = List(
    ("type",Str("endpoints")),
    ("information",Str(html.toString)),
    ("endpoints",JsArray(endpoints.map(ep => JsObj(List(
      ("url",Str(ep.endpoint)),
      ("name",Str(ep.name)),
      ("description",Str(ep.description))
    ):_*))))
  )
}

case class EndpointDescriptor(name:String,endpoint:String,description:String)

case object NullCheck extends VisualElement {
	override val label = "null"
	override val getServerName:String = "null"
	override val getServiceName:String = "null"
}

abstract class Pinger(incomingName:String,incomingLabel:String,incomingMode:ServiceCheckMode,incomingSeverity:ServiceCheckSeverity) extends LiftActor with VisualElement with metl.comet.CheckRenderHelper with Logger {
  import GraphableData._
	val mode: ServiceCheckMode = incomingMode
	val severity: ServiceCheckSeverity = incomingSeverity
	val name: String = incomingName
	override val label: String = incomingLabel
	override def template: NodeSeq = ViewTemplates.getServiceTemplate
	var checkTimeout:Box[TimeSpan] = Empty
	var failureTolerance = 1
	var lastCheckBegin:Box[Date] = Empty
	var lastUptime:Box[Date] = Empty
	def lastUptimeString: String = lastUptime.map(t => t.toString).openOr("NEVER")
	var lastCheck:Box[Date] = Empty
	var lastStatus:Box[Boolean] = Empty
	var lastWhy:Box[String] = Empty
	var lastDetail:Box[String] = Empty
	var currentFailures = 0
	def lastStatusClass: String = statusClassFromStatus(lastStatus)
	def lastStatusCode: String = statusCodeFromStatus(lastStatus)
  protected val pollInterval: Helpers.TimeSpan = 5 seconds
  private def updatedTime(success:Boolean,now:Date = new Date()):Date = {
    val now = new Date()
		lastCheck = Full(now)
		if (success){
			lastUptime = Full(now)
		}
    now
  }
	protected def internalResetEnvironment = {
		resetEnvironment
		isPerformingCheck = false
	}
  protected def resetEnvironment = { }
  def schedule(interval:TimeSpan = pollInterval) = {
    Schedule.schedule(this,Check,interval)
  }
  def fail(why:String,detail:String = "") = {
		val lastUp = lastUptime
		val now = updatedTime(false)
		lastStatus = Full(false)
		currentFailures = currentFailures + 1
    val cr = CheckResult(id,name,label,getServiceName,getServerName,now,why,lastUp,detail,mode,severity,false)
    DashboardServer ! cr
    HistoryServer ! cr
		if (currentFailures >= failureTolerance){
			ErrorRecorder ! cr
		}
  }
  def succeed(why:String,timeTaken:Box[Double] = Empty,data:List[Tuple2[Long,Map[String,GraphableDatum]]] = Nil) = {
		val now = new Date()
		val checkDuration = timeTaken.openOr((new Date().getTime - lastCheckBegin.openOr(now).getTime).toDouble)
		checkTimeout.map(c => {
			if (checkDuration >= c.millis){
				throw new DashboardException("Timeout","This check passed, but took %sms when it is not permitted to take %s or longer: %s".format(checkDuration, c, why))
			}
		})
		val lastUp = lastUptime
		updatedTime(true,now)
		lastStatus = Full(true)
		currentFailures = 0
    var cr =  CheckResult(id,name,label,getServiceName,getServerName,now,why,lastUp,"",mode,severity,true,data)
    HistoryServer ! cr
		DashboardServer ! cr 
		ErrorRecorder ! cr 
  }
  override protected def exceptionHandler:PartialFunction[Throwable,Unit] = {
		case DashboardException(reason,detail,innerExceptions) => {
			fail(reason,detail)
			internalResetEnvironment
			schedule()	
		}
    case t:Throwable =>{
      fail(t.toString)
      internalResetEnvironment
      schedule()
    }
    case _ => {}
  }
	override def templateRenderer = {
		val (why,detail) = lastStatus.map(s => s match {
			case true => (lastWhy.openOr("").take(500),lastDetail.openOr("").take(500))
			case false => (lastWhy.openOr(""),lastDetail.openOr(""))
		}).openOr(("",""))
		val tooltip = lastStatusCode + ": "+label+" (@"+now.toString+") : "+why
		(
			".serviceCapacity *" #> label &
			".serviceLastChecked *" #> now.toString &
			".serviceLastUp *" #> lastUptimeString &
			".serviceStatus *" #> lastStatusCode &
			".serviceStatus [title]" #> tooltip &
			".serviceStatus [class+]" #> lastStatusClass &
			".serviceLastUp *" #> lastUptimeString &	
			".serviceClass *" #> mode.toString &
			".serviceWhy *" #> why &
			".serviceDetail *" #> detail &
			".servicePeriod *" #> pollInterval.toString 
		)
	}
  override def asJson = {
		val (why,detail) = lastStatus.map(s => s match {
			case true => (lastWhy.openOr("").take(500),lastDetail.openOr("").take(500))
			case false => (lastWhy.openOr(""),lastDetail.openOr(""))
		}).openOr(("",""))
		val lastCheckTime:Long = lastCheck.map(_.getTime()).openOr(0L)
    List(
      JField("type",JString("pinger")),
      JField("lastChecked",JString(lastCheck.map(d => d.toString).openOr("NEVER"))),
      JField("lastSuccess",JBool(lastStatus.openOr(false))),
      JField("lastStatusCode",JString(lastStatusCode)),
      JField("lastUp",JString(lastUptimeString)),
			JField("lastCheck",JInt(lastCheckTime)),
			JField("period",JInt(pollInterval.millis)),
      JField("mode",JString(mode.toString)),
      JField("severity",JString(severity.toString)),
			JField("lastWhy",JString(why)),
      JField("lastDetail",JString(detail)),
      JField("pollInterval",JString(pollInterval.toString))
    )
  }
  override def asJsExp = {
		val (why,detail) = lastStatus.map(s => s match {
			case true => (lastWhy.openOr("").take(500),lastDetail.openOr("").take(500))
			case false => (lastWhy.openOr(""),lastDetail.openOr(""))
		}).openOr(("",""))
    List(
      ("type",Str("pinger")),
      ("lastChecked",Str(lastCheck.map(d => d.toString).openOr("NEVER"))),
      ("lastSuccess",Str(lastStatus.openOr(false).toString)),
      ("lastStatusCode",Str(lastStatusCode)),
      ("lastUp",Str(lastUptimeString)),
			("lastCheck",Num(lastCheck.map(_.getTime()).getOrElse(0L))),
			("period",Num(pollInterval.millis)),
			("mode",Str(mode.toString)),
			("severity",Str(severity.toString)),
			("lastWhy",Str(why)),
			("lastDetail",Str(detail)),
      ("pollInterval",Str(pollInterval.toString))
    )    
  }
	private var isStopped = true
	def isRunning:Boolean = !isStopped
	protected def performCheck = {}
	private var isPerformingCheck = false
	protected def privatePerformCheck = {
		if (!isPerformingCheck){
			isPerformingCheck = true
			lastCheckBegin = Full(new Date())
			performCheck
			isPerformingCheck = false
		}
	}
  override def messageHandler = {
    case Check =>{
			if (!isStopped){
				privatePerformCheck
				schedule()
			}
    }
		case StopPinger => {
			if (!isStopped){
				debug("stopping pinger: %s:%s (%s)".format(label,mode,id))
				isStopped = true
			}
		}
		case StartPinger => {
			if (isStopped){
				debug("starting pinger: %s:%s (%s)".format(label,mode,id))
				isStopped = false
				resetEnvironment
				schedule(2 seconds)
			}
		}
    case _ => {}
  }
}
case class CheckUnexceptional(serviceCheckMode:ServiceCheckMode,serviceCheckSeverity:ServiceCheckSeverity, incomingName:String,incomingLabel:String,condition:Function0[Any],time:TimeSpan = 5 seconds) extends Pinger(incomingName,incomingLabel,serviceCheckMode,serviceCheckSeverity){
  override val pollInterval = time
  def status = condition()
	override def performCheck = succeed("Was expected")
}
case class CheckDoesnt(serviceCheckMode:ServiceCheckMode,serviceCheckSeverity:ServiceCheckSeverity, incomingName:String,incomingLabel:String,condition:Function0[Option[String]], time:TimeSpan = 5 seconds) extends Pinger(incomingName,incomingLabel,serviceCheckMode,serviceCheckSeverity){
  override val pollInterval = 5 seconds
	override def performCheck = {
		condition() match {
      case None =>{
        succeed("Ok")
      }
      case Some(error) => throw new DashboardException("checkDoesn't failed",error)
		}
	}
}
case class MatcherCheck(serviceCheckMode:ServiceCheckMode,serviceCheckSeverity:ServiceCheckSeverity,incomingName:String,incomingLabel:String,matcher:Matcher,time:TimeSpan) extends Pinger(incomingName,incomingLabel,serviceCheckMode,serviceCheckSeverity){
	override val pollInterval = time
	failureTolerance = 3
	def status = "%s is %s".format(matcher.describe,matcher.verify(true).toString)
	override def performCheck = succeed(status)
}
