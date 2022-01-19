package metl.model

import metl.comet._
import net.liftweb._
import net.liftweb.actor._
import net.liftweb.common._
import http._
import js._
import json.JsonAST._
import util.{Helpers, _}
import Helpers._

import xml._
import java.util.Date

import net.liftweb.common.Logger

case class DashboardException(reason: String,
                              detail: String,
                              exceptions: List[Exception] = Nil)
    extends Exception(reason) {
  override def toString: String = {
    "Exception(%s, %s)".format(reason, detail)
  }
}
case object Check
case object StartSensor
case object StopSensor

trait VisualElement {
  val id: String = nextFuncName
  val label: String
  val serviceName: String
  val serviceLabel: String
  val serverName: String
  val serverLabel: String
  def jsonRenderer(sen: String = serviceName,
                   sel: String = serviceLabel,
                   srn: String = serverName,
                   srl: String = serverLabel): JObject = {
    JObject(
      List(
        JField("id", JString(id)),
        JField("serviceName", JString(sen)),
        JField("serviceLabel", JString(sel)),
        JField("serverName", JString(srn)),
        JField("serverLabel", JString(srl)),
        JField("label", JString(label))
      ) ::: asJson)
  }
  protected def asJson: List[JField] = Nil
}

case class HtmlInformation(metadata: SensorMetaData,
                           name: String,
                           html: NodeSeq)
    extends VisualElement {
  override val serviceName: String = metadata.serviceName
  override val serviceLabel: String = metadata.serviceLabel
  override val serverName: String = metadata.serverName
  override val serverLabel: String = metadata.serverLabel
  override val label: String = "%s information".format(name)
  override def asJson = List(
    JField("type", JString("htmlInformation")),
    JField("html", JString(html.toString))
  )
}

case class Information(metadata: SensorMetaData, name: String, message: String)
    extends VisualElement {
  override val serviceName: String = metadata.serviceName
  override val serviceLabel: String = metadata.serviceLabel
  override val serverName: String = metadata.serverName
  override val serverLabel: String = metadata.serverLabel
  override val label: String = name
  override def asJson = List(
    JField("type", JString("information")),
    JField("information", JString(message))
  )
}

case class ErrorInformation(metadata: SensorMetaData,
                            name: String,
                            expectedPeriod: String,
                            sourceString: String,
                            errors: List[String])
    extends VisualElement {
  override val serviceName: String = metadata.serviceName
  override val serviceLabel: String = metadata.serviceLabel
  override val serverName: String = metadata.serverName
  override val serverLabel: String = metadata.serverLabel
  override val label: String = "Error: %s".format(name)
  override def asJson = List(
    JField("type", JString("error")),
    JField("source", JString(sourceString)),
    JField("expectedPeriod", JString(expectedPeriod)),
    JField("errors", JArray(errors.map(e => JString(e))))
  )
}

case class EndpointInformationWithString(metadata: SensorMetaData,
                                         name: String,
                                         htmlDescriptor: String,
                                         endpoints: List[EndpointDescriptor])
    extends EndpointInformationWithHtml(metadata,
                                        name,
                                        Text(htmlDescriptor),
                                        endpoints) {
  override val serviceName: String = metadata.serviceName
  override val serviceLabel: String = metadata.serviceLabel
  override val serverName: String = metadata.serverName
  override val serverLabel: String = metadata.serverLabel
}
class EndpointInformationWithHtml(metadata: SensorMetaData,
                                  name: String,
                                  html: NodeSeq,
                                  endpoints: List[EndpointDescriptor])
    extends VisualElement {
  override val serviceName: String = metadata.serviceName
  override val serviceLabel: String = metadata.serviceLabel
  override val serverName: String = metadata.serverName
  override val serverLabel: String = metadata.serverLabel
  override val label: String = name
  override def asJson = List(
    JField("type", JString("endpoints")),
    JField("information", JString(html.toString)),
    JField(
      "endpoints",
      JArray(
        endpoints.map(
          ep =>
            JObject(
              List(
                JField("url", JString(ep.endpoint)),
                JField("name", JString(ep.name)),
                JField("description", JString(ep.description))
              ))))
    )
  )
}

case class EndpointDescriptor(name: String,
                              endpoint: String,
                              description: String)

case object NullCheck extends VisualElement {
  override val serviceName: String = "null"
  override val serviceLabel: String = "null"
  override val serverName: String = "null"
  override val serverLabel: String = "null"
  override val label = "null"
}

case class SensorMetaData(name: String,
                          label: String,
                          mode: ServiceCheckMode,
                          severity: ServiceCheckSeverity,
                          serviceName: String,
                          serviceLabel: String,
                          serverName: String,
                          serverLabel: String,
                          expectFail: Boolean = false,
                          timeout: Option[TimeSpan] = None,
                          acceptedFailures: Int = 1)

abstract class Sensor(metadata: SensorMetaData)
    extends LiftActor
    with VisualElement
    with metl.comet.CheckRenderHelper
    with Logger {
  import GraphableData._
  override val serviceName: String = metadata.serviceName
  override val serviceLabel: String = metadata.serviceLabel
  override val serverName: String = metadata.serverName
  override val serverLabel: String = metadata.serverLabel
  val mode: ServiceCheckMode = metadata.mode
  val severity: ServiceCheckSeverity = metadata.severity
  val name: String = metadata.name
  override val label: String = metadata.label
  var checkTimeout: Box[TimeSpan] = metadata.timeout
  var checkDuration: Box[Double] = Empty
  var failureTolerance: Int = metadata.acceptedFailures
  var lastCheckBegin: Box[Date] = Empty
  var lastUptime: Box[Date] = Empty
  var lastCheck: Box[Date] = Empty
  var lastStatus: Box[Boolean] = Empty
  var lastWhy: Box[String] = Empty
  var lastDetail: Box[String] = Empty
  var currentFailures = 0

  protected val maxHistory = 5
  protected var history: List[CheckResult] = Nil
  protected def addCheckResult(cr: CheckResult,
                               shouldSendToError: Boolean = true) = {
    history = cr :: history.take(maxHistory - 1)
    DashboardServer ! cr
    HistoryServer ! cr
    if (shouldSendToError) {
      ErrorRecorder ! cr
    }
  }
  protected val pollInterval: Helpers.TimeSpan = 5 seconds
  private def updatedTime(success: Boolean, now: Date = new Date()): Date = {
    val now = new Date()
    lastCheck = Full(now)
    if (success) {
      lastUptime = Full(now)
    }
    now
  }
  protected def internalResetEnvironment = {
    resetEnvironment
    isPerformingCheck = false
  }
  protected def resetEnvironment = {}
  def schedule(interval: TimeSpan = pollInterval) = {
    Schedule.schedule(this, Check, interval)
  }
  def fail(why: String, detail: String = "", timeTaken: Box[Double] = Empty) = {
    val lastUp = lastUptime
    val now = updatedTime(success = false)
    lastStatus = Full(false)
    currentFailures = currentFailures + 1
    val durationOrTimeSinceStart = calculateCheckDuration(timeTaken)
    checkDuration = Full(durationOrTimeSinceStart)
    val cr = CheckResult(id,
                         name,
                         label,
                         serviceName,
                         serviceLabel,
                         serverName,
                         serverLabel,
                         now,
                         why,
                         lastUp,
                         detail,
                         mode,
                         severity,
                         success = false,
                         duration = checkDuration)
    addCheckResult(cr, currentFailures >= failureTolerance)

  }
  def calculateCheckDuration(timeTaken: Box[Double] = Empty): Double = {
    val now = new Date()
    timeTaken.openOr(
      (now.getTime - lastCheckBegin.openOr(now).getTime).toDouble)
  }
  def succeed(why: String,
              timeTaken: Box[Double] = Empty,
              data: List[Tuple2[Long, Map[String, GraphableDatum]]] = Nil) = {
    val now = new Date()
    val durationOrTimeSinceStart = calculateCheckDuration(timeTaken)
    checkDuration = Full(durationOrTimeSinceStart)
    checkTimeout.map(c => {
      if (durationOrTimeSinceStart >= c.millis) {
        throw DashboardException(
          "Timeout",
          "This check passed, but took %sms when it is not permitted to take %s or longer: %s"
            .format(checkDuration, c, why))
      }
    })
    val lastUp = lastUptime
    updatedTime(success = true, now)
    lastStatus = Full(true)
    currentFailures = 0
    val cr = CheckResult(id,
                         name,
                         label,
                         serviceName,
                         serviceLabel,
                         serverName,
                         serverLabel,
                         now,
                         why,
                         lastUp,
                         "",
                         mode,
                         severity,
                         success = true,
                         data,
                         checkDuration)
    addCheckResult(cr)
  }
  override protected def exceptionHandler: PartialFunction[Throwable, Unit] = {
    case DashboardException(reason, detail, innerExceptions) => {
      fail(reason,
           detail,
           lastCheckBegin.map(d => (new Date().getTime - d.getTime).toDouble))
      internalResetEnvironment
      schedule()
    }
    case t: Throwable => {
      fail(t.toString,
           timeTaken =
             lastCheckBegin.map(d => (new Date().getTime - d.getTime).toDouble))
      internalResetEnvironment
      schedule()
    }
    case _ => {}
  }
  override def asJson = {
    val (why, detail) = lastStatus
      .map(s =>
        s match {
          case true =>
            (lastWhy.openOr("").take(500), lastDetail.openOr("").take(500))
          case false => (lastWhy.openOr(""), lastDetail.openOr(""))
      })
      .openOr(("", ""))
    List(
      JField("type", JString("pinger")),
      JField("name", JString(name)),
      JField("period", JInt(pollInterval.millis)),
      JField("mode", JString(mode.toString)),
      JField("severity", JString(severity.toString)),
      //JField("lastWhy",JString(why)),
      //JField("lastDetail",JString(detail)),
      JField("expectFail", JBool(metadata.expectFail)),
      JField("history",
             JArray(history.drop(1).map(c => JObject(c.generateJson))))
    ) ::: history.headOption.toList.flatMap(h => {
      h.generateJson
    }) //::: lastUptime.map(lu => JField("lastUp",JInt(lu.getTime))).toList :::
    //lastStatus.map(ls => JField("status",JBool(ls))).toList :::
    //lastCheck.map(lc => JField("lastCheck",JInt(lc.getTime()))).toList
  }
  private var isStopped = true
  def isRunning: Boolean = !isStopped
  protected def performCheck = {}
  private var isPerformingCheck = false
  protected def privatePerformCheck = {
    if (!isPerformingCheck) {
      isPerformingCheck = true
      lastCheckBegin = Full(new Date())
      performCheck
      isPerformingCheck = false
    }
  }
  override def messageHandler = {
    case Check => {
      if (!isStopped) {
        privatePerformCheck
        schedule()
      }
    }
    case StopSensor => {
      if (!isStopped) {
        trace("stopping pinger: %s:%s (%s)".format(label, mode, id))
        DashboardServer ! RemoveCheck(this)
        isStopped = true
      }
    }
    case StartSensor => {
      if (isStopped) {
        trace("starting pinger: %s:%s (%s)".format(label, mode, id))
        isStopped = false
        resetEnvironment
        DashboardServer ! CreateCheck(this)
        schedule(2 seconds)
      }
    }
    case _ => {}
  }
}
case class CheckUnexceptional(metadata: SensorMetaData,
                              condition: Function0[Any],
                              time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  def status = condition()
  override def performCheck = succeed("Was expected")
}
case class CheckDoesnt(metadata: SensorMetaData,
                       condition: Function0[Option[String]],
                       time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = 5 seconds
  override def performCheck = {
    condition() match {
      case None => {
        succeed("Ok")
      }
      case Some(error) =>
        throw new DashboardException("checkDoesn't failed", error)
    }
  }
}
case class MatcherCheck(metadata: SensorMetaData,
                        matcher: Matcher,
                        time: TimeSpan)
    extends Sensor(metadata) {
  override val pollInterval = time
  failureTolerance = 3
  def status =
    "%s is %s".format(matcher.describe, matcher.verify(true).toString)
  override def performCheck = succeed(status)
}
/*
case class InvertedCheck(pinger:Pinger) extends Pinger(pinger.name,pinger.label,pinger.mode,pinger.severity) {
        override def performCheck = {
                pinger.performCheck
        }
}
 */
