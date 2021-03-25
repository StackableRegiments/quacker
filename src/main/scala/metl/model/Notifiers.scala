package metl.model

import org.apache.commons.io.IOUtils
import net.liftweb._
import net.liftweb.actor._
import net.liftweb.common._
import http._
import util._
import Helpers._
import net.liftweb.http.SHtml._
import java.util.Date

import collection.JavaConverters._
import net.liftweb.common.Logger
import net.liftweb.util.TimeHelpers
//file writer
import java.io._

import metl.comet._

import scala.xml._
import net.liftweb.json._

abstract class ServiceCheckMode
case object STAGING extends ServiceCheckMode
case object PRODUCTION extends ServiceCheckMode
case object TEST extends ServiceCheckMode
case object DEVELOPMENT extends ServiceCheckMode
case object OPERATIONS extends ServiceCheckMode

object ServiceCheckMode {
  def parse(s: String): ServiceCheckMode = s.toLowerCase.trim match {
    case "staging"     => STAGING
    case "production"  => PRODUCTION
    case "test"        => TEST
    case "development" => DEVELOPMENT
    case "operations"  => OPERATIONS
    case _             => TEST
  }
}

abstract class ServiceCheckSeverity
case object IMPACT extends ServiceCheckSeverity
case object ISSUE extends ServiceCheckSeverity
case object ALERT extends ServiceCheckSeverity

object ServiceCheckSeverity {
  def parse(s: String): ServiceCheckSeverity = s.toLowerCase.trim match {
    case "impact" => IMPACT
    case "issue"  => ISSUE
    case "alert"  => ALERT
    case _        => ALERT
  }
}

sealed trait GraphableDatum {
  def getAsString: String
}
case class GraphableString(v: String) extends GraphableDatum {
  override def getAsString: String = v
}
case class GraphableLong(v: Long) extends GraphableDatum {
  override def getAsString: String = v.toString
}
case class GraphableInt(v: Int) extends GraphableDatum {
  override def getAsString: String = v.toString
}
case class GraphableDouble(v: Double) extends GraphableDatum {
  override def getAsString: String = v.toString
}
case class GraphableFloat(v: Float) extends GraphableDatum {
  override def getAsString: String = v.toString
}
case class GraphableBoolean(v: Boolean) extends GraphableDatum {
  override def getAsString: String = v.toString
}

object GraphableStringSerializer
    extends CustomSerializer[GraphableString](
      (formats: net.liftweb.json.Formats) =>
        ({
          case JString(s) => GraphableString(s)
        }, {
          case GraphableString(s) => JString(s)
        }))
object GraphableIntSerializer
    extends CustomSerializer[GraphableInt](
      (formats: net.liftweb.json.Formats) =>
        ({
          case JInt(s) => GraphableInt(s.toInt)
        }, {
          case GraphableInt(s) => JInt(s)
        }))
object GraphableLongSerializer
    extends CustomSerializer[GraphableLong](
      (formats: net.liftweb.json.Formats) =>
        ({
          case JInt(s) => GraphableLong(s.toLong)
        }, {
          case GraphableLong(s) => JInt(s)
        }))
object GraphableFloatSerializer
    extends CustomSerializer[GraphableFloat](
      (formats: net.liftweb.json.Formats) =>
        ({
          case JDouble(s) => GraphableFloat(s.toFloat)
        }, {
          case GraphableFloat(s) => JDouble(s)
        }))
object GraphableDoubleSerializer
    extends CustomSerializer[GraphableDouble](
      (formats: net.liftweb.json.Formats) =>
        ({
          case JDouble(s) => GraphableDouble(s.toDouble)
        }, {
          case GraphableDouble(s) => JDouble(s)
        }))
object GraphableBooleanSerializer
    extends CustomSerializer[GraphableBoolean](
      (formats: net.liftweb.json.Formats) =>
        ({
          case JBool(s) => GraphableBoolean(s)
        }, {
          case GraphableBoolean(s) => JBool(s)
        }))
object GraphableDatumSerializer
    extends CustomSerializer[GraphableDatum](
      (formats: net.liftweb.json.Formats) =>
        ({
          case JString(s) => GraphableString(s)
          case JInt(s)    => GraphableLong(s.toLong)
          case JDouble(s) => GraphableDouble(s.toDouble)
          case JBool(s)   => GraphableBoolean(s)
        }, {
          case GraphableBoolean(s) => JBool(s)
          case GraphableDouble(s)  => JDouble(s)
          case GraphableFloat(s)   => JDouble(s)
          case GraphableLong(s)    => JInt(s)
          case GraphableInt(s)     => JInt(s)
          case GraphableString(s)  => JString(s)
        }))

object GraphableData {
  val formats = net.liftweb.json.DefaultFormats + GraphableDatumSerializer + GraphableDoubleSerializer + GraphableFloatSerializer + GraphableLongSerializer + GraphableIntSerializer + GraphableBooleanSerializer
  implicit def convert(in: String) = GraphableString(in)
  implicit def convert(in: Long) = GraphableLong(in)
  implicit def convert(in: Int) = GraphableInt(in)
  implicit def convert(in: Double) = GraphableDouble(in)
  implicit def convert(in: Float) = GraphableFloat(in)
  implicit def convert(in: Boolean) = GraphableBoolean(in)
}

case class CheckResult(id: String,
                       serviceCheck: String,
                       label: String,
                       service: String,
                       serviceLabel: String,
                       server: String,
                       serverLabel: String,
                       when: Date,
                       why: String,
                       lastUp: Box[Date],
                       detail: String,
                       mode: ServiceCheckMode,
                       severity: ServiceCheckSeverity,
                       success: Boolean,
                       data: List[Tuple2[Long, Map[String, GraphableDatum]]] =
                         Nil,
                       duration: Box[Double] = Empty) {
  def generateJson: List[JField] = {
    List(
      JField("id", JString(id)),
      JField("status", JBool(success)),
      JField("label", JString(label)),
      JField("now", JInt(when.getTime())),
      JField("why", JString(why)),
      JField("detail", JString(success match {
        case true => ""
        case _    => detail
      })),
      JField("lastCheck", JInt(when.getTime())),
      JField(
        "data",
        JArray(data.map(tup => {
          JObject(
            List(
              JField("when", JInt(tup._1)),
              JField(
                "values",
                JObject(tup._2.toList.map(dTup => {
                  JField(
                    dTup._1,
                    dTup._2 match {
                      case GraphableFloat(f)   => JDouble(f)
                      case GraphableDouble(d)  => JDouble(d)
                      case GraphableInt(i)     => JInt(i)
                      case GraphableLong(l)    => JInt(l)
                      case GraphableString(s)  => JString(s)
                      case GraphableBoolean(b) => JBool(b)
                    }
                  )
                }))
              )
            ))
        }))
      )
    ) ::: duration.map(d => JField("duration", JDouble(d))).toList ::: lastUp
      .map(lu => JField("lastUp", JInt(lu.getTime())))
      .toList

  }
}

abstract class ErrorActor(name: String) extends LiftActor {
  protected def outputAction(cr: CheckResult) = {
    cr.success match {
      case false => registerFailure(cr)
      case true  => registerSuccess(cr)
    }
  }
  protected val filterAction: (CheckResult) => Boolean = (c: CheckResult) =>
    true
  override def messageHandler = {
    case c: CheckResult if filterAction(c) => outputAction(c)
    case _                                 => {}
  }
  protected def registerFailure(cr: CheckResult): Unit
  protected def registerSuccess(cr: CheckResult): Unit
}

abstract class LogEveryErrorActor(name: String) extends ErrorActor(name) {}

abstract class LogChangesErrorActor(name: String) extends ErrorActor(name) {
  protected val initialInterval: Long = 5000L
  protected val exponentialFactor: Int = 2
  protected val maximumInterval: Long = 3600000L
  private val recentActions =
    new scala.collection.mutable.HashMap[String,
                                         Map[String, Tuple2[Long, Long]]] {
      override def default(who: String) = {
        Map.empty[String, Tuple2[Long, Long]]
      }
    }
  private def updateRecentActions(who: String,
                                  why: String,
                                  when: Long): Unit = {
    val interval = tryo(recentActions(who)(why)._2).openOr(initialInterval)
    recentActions.update(
      who,
      recentActions(who).updated(
        why,
        (when, Math.min(interval * exponentialFactor, maximumInterval))))
  }
  private def shouldFail(who: String, why: String, when: Long): Boolean = {
    val lastMail = tryo(recentActions(who)(why)).openOr((0L, initialInterval))
    ((when - lastMail._1) > lastMail._2)
  }
  override def registerFailure(cr: CheckResult): Unit = {
    val who = cr.label
    val serviceName = cr.service
    val serverName = cr.server
    val why = cr.why
    val date = cr.when
    val detail = cr.detail
    val lastUp = cr.lastUp.map(lu => lu.toString).openOr("NEVER")
    val mode = cr.mode
    val when = date.getTime
    if (shouldFail(who, why, when)) {
      doFailureAction(who,
                      serviceName,
                      serverName,
                      why,
                      detail,
                      date,
                      lastUp,
                      mode)
      updateRecentActions(who, why, when)
    }
  }
  override def registerSuccess(cr: CheckResult): Unit = {
    val who = cr.label
    val serviceName = cr.service
    val serverName = cr.server
    val date = cr.when
    val lastUp = cr.lastUp.map(lu => lu.toString).openOr("NEVER")
    val mode = cr.mode
    if (recentActions(who).keys.toList.length > 0) {
      doSuccessAction(who, serviceName, serverName, date, lastUp, mode)
      recentActions.update(who, Map.empty[String, Tuple2[Long, Long]])
    }
  }
  protected def doSuccessAction(who: String,
                                serviceName: String,
                                serverName: String,
                                date: Date,
                                lastUp: String,
                                mode: ServiceCheckMode): Unit = {}
  protected def doFailureAction(who: String,
                                serviceName: String,
                                serverName: String,
                                why: String,
                                detail: String,
                                date: Date,
                                lastUp: String,
                                mode: ServiceCheckMode): Unit = {}
}

class AppendableFile(filename: String) extends Logger {
  private var box: Box[FileWriter] = Empty
  private def setupBox = {
    box = box match {
      case Empty => {
        try {
          Full(new FileWriter(filename, true))
        } catch {
          case e: java.io.IOException => {
            error("failed to create filewriter with expected IOException: %s"
                    .format(e.getMessage.toString),
                  e)
            Empty
          }
          case e: Throwable => {
            error("failed to create filewriter with unexpected exception: %s"
                    .format(e.getMessage.toString),
                  e)
            Empty
          }
        }
      }
      case Full(fw) => {
        try {
          fw.close
          Full(new FileWriter(filename, true))
        } catch {
          case e: java.io.IOException => {
            error(
              "failed to close filewriter with expected IOException: %s".format(
                e.getMessage.toString),
              e)
            Empty
          }
          case e: Throwable => {
            error(
              "failed to close filewriter with unexpected exception: %s".format(
                e.getMessage.toString),
              e)
            Empty
          }
        }
      }
      case other => {
        error("other error on box: %s".format(other))
        Empty
      }
    }
  }
  def writeLine(s: String) = {
    setupBox
    box.map(fw => {
      fw.write(s + "\r\n")
      fw.close
    })
  }
}

class ErrorDiskLogger(name: String, filename: String)
    extends LogEveryErrorActor(name) {
  private val file = new AppendableFile(filename)
  override def registerFailure(cr: CheckResult) = writeMessage(cr)
  override def registerSuccess(cr: CheckResult) = writeMessage(cr)
  def writeMessage(input: CheckResult): Unit = {
    List(
      "label : %s".format(input.label),
      "when : %s".format(input.when.toString),
      "result : %s".format(input.success.toString),
      "why : %s".format(input.why),
      "lastUp : %s".format(input.lastUp.map(lu => lu.toString).openOr("NEVER")),
      "detail : %s".format(input.detail),
      "---"
    ).foreach(l => file.writeLine(l))
  }
}

case class SimpleMailer(smtp: String,
                        port: Int,
                        ssl: Boolean,
                        username: String,
                        password: String,
                        fromAddress: Option[String] = None)
    extends net.liftweb.util.Mailer
    with Logger {
  import net.liftweb.util.Mailer._
  customProperties = Map(
    "mail.smtp.starttls.enable" -> ssl.toString,
    "mail.smtp.host" -> smtp,
    "mail.smtp.port" -> port.toString,
    "mail.smtp.auth" -> "true"
  )
  authenticator = Full(new javax.mail.Authenticator {
    override def getPasswordAuthentication =
      new javax.mail.PasswordAuthentication(username, password)
  })
  def sendMailMessage(to: String,
                      who: String,
                      subject: String,
                      message: String): Unit = {
    try {
			trace("sendingMailMessage to:%s, from:%s, subject:%s, message:%s".format(fromAddress,subject,message,to))
      sendMail(
        From(fromAddress.getOrElse("Service.Monitor@stackableregiments.com")),
        Subject(subject),
        PlainMailBodyType(message) :: List(To(to)): _*)
    } catch {
      case e: Throwable => {
        error("exception while sending mail: to:%s from: %s, %s".format(fromAddress, to, e.getMessage), e)
      }
    }
  }
}

class ErrorMailer(name: String,
                  smtp: String,
                  port: Int,
                  username: String,
                  password: String,
                  fromAddress: Option[String] = None)
    extends LogChangesErrorActor(name) {
  protected val ssl: Boolean = true
  protected lazy val mailer =
    SimpleMailer(smtp, port, ssl, username, password, fromAddress)
  protected val messagePrefix: String = ""
  protected val messageSuffix: String = ""
  protected val messageSubject: String = "alert"
  protected val interestedParties: List[String] = List.empty[String]
  protected val shortcutHost: String = ""

  protected def successSubject(mode: ServiceCheckMode,
                               serviceName: String,
                               serverName: String): String =
    "%s PASS %s %s %s"
      .format(modeContractor(mode), messageSubject, serviceName, serverName)
      .take(155)
      .toString
  protected def failureSubject(mode: ServiceCheckMode,
                               serviceName: String,
                               serverName: String): String =
    "%s FAIL %s %s %s"
      .format(modeContractor(mode), messageSubject, serviceName, serverName)
      .take(155)
      .toString

  protected def modeContractor(mode: ServiceCheckMode): String = mode match {
    case PRODUCTION  => "PRD"
    case STAGING     => "QAT"
    case DEVELOPMENT => "DEV"
    case TEST        => "TST"
    case _           => ""
  }

  override def doSuccessAction(who: String,
                               serviceName: String,
                               serverName: String,
                               date: Date,
                               lastUp: String,
                               mode: ServiceCheckMode): Unit = {
    val successMessage = """%s%s : %s 
SUCCESS: '%s'  [%s].

%s/?expandedServices=%s&expandedChecks=%s

%s Detection time.
%s Last known uptime.
%s""".format(messagePrefix,
             serviceName,
             serverName,
             who,
             mode.toString,
             shortcutHost,
             urlEncode(serviceName),
             urlEncode(who),
             date,
             lastUp,
             messageSuffix)
    sendMailMessage(who,
                    successSubject(mode, serviceName, serverName),
                    successMessage)
  }
  override def doFailureAction(who: String,
                               serviceName: String,
                               serverName: String,
                               why: String,
                               detail: String,
                               date: Date,
                               lastUp: String,
                               mode: ServiceCheckMode): Unit = {
    val failMessage = """%s%s : %s
FAIL: '%s'  [%s].
Error message: '%s'.

%s/?expandedServices=%s&expandedChecks=%s

%s Detection time.
%s Last known uptime.

Error detail: '%s'.

%s""".format(messagePrefix,
             serviceName,
             serverName,
             who,
             mode.toString,
             why,
             shortcutHost,
             urlEncode(serviceName),
             urlEncode(who),
             date,
             lastUp,
             detail,
             messageSuffix)
    sendMailMessage(who,
                    failureSubject(mode, serviceName, serverName),
                    failMessage)
  }
  protected def sendMailMessage(who: String,
                                subject: String,
                                message: String): Unit =
    interestedParties.foreach(emailAddress =>
      mailer.sendMailMessage(emailAddress, who, subject, message))
}

object ErrorRecorder extends LiftActor with ConfigFileReader {
  var mailers = List.empty[LiftActor]
  def clear = {
    mailers = List.empty[LiftActor]
  }
  def configureFromXml(xml: Node): List[String] = {
    val diskLoggers = (xml \\ "diskLogger")
      .map(n => {
        val name = getText(n, "name").getOrElse("")
        val file = getText(n, "file").getOrElse(
          throw new Exception(
            "no file name specified for disklogger: %s".format(n.toString)))
        val level = getNodes(n, "levels").headOption.getOrElse(
          <error>No suitable levels found</error>)
        val servicePermissions: List[ServicePermission] =
          getNodes(n, "servicePermissions")
            .map(spNodes =>
              getNodes(spNodes, "service").map(sp =>
                ServicePermission.configureFromXml(sp)))
            .flatten
            .toList
        val restrictions = UserAccessRestriction(name, servicePermissions)
        val filterFunc = (cr: CheckResult) => restrictions.permit(cr)
        val edl = new ErrorDiskLogger(name, file) {
          override val filterAction = (cr: CheckResult) => filterFunc(cr)
        }
        trace("creating ErrorDiskLogger: %s (%s)".format(name, edl))
        edl
      })
      .toList
    val emailLoggers = (xml \\ "mailer")
      .map(n => {
        val name = getText(n, "name").getOrElse("")
        val smtp = getText(n, "smtp").getOrElse("")
        val port = getInt(n, "port").getOrElse(0)
        val configSsl = getBool(n, "ssl").getOrElse(false)
        val username = getText(n, "username").getOrElse("")
        val password = getText(n, "password").getOrElse("")
        val xmlInterval = getLong(n, "initialInterval")
          .map(i => i.toInt.toLong)
          .getOrElse(5000L)
        val xmlGrowthFactor =
          getInt(n, "exponentialFactor").map(i => i.toInt).getOrElse(2)
        val xmlMaxInterval = getLong(n, "maximumInterval").getOrElse(3600000L)
        val shortcutLinkServerName = getText(n, "shortcutHost").getOrElse("")
        val xmlMessageSubject = getText(n, "mailSubject").getOrElse("")
        val xmlFromAddress = getText(n, "mailFrom")
        val xmlMessagePrefix = getText(n, "mailMessageBodyPrefix").getOrElse("")
        val xmlMessageSuffix = getText(n, "mailMessageBodySuffix").getOrElse("")
        val xmlInterestedParties = getNodes(n, "recipients")
          .map(
            rNode =>
              getNodes(rNode, "emailAddress")
                .map(eNode => eNode.text.toString)
                .filterNot(emailAddress => emailAddress == ""))
          .flatten
          .toList
        val servicePermissions: List[ServicePermission] =
          getNodes(n, "servicePermissions")
            .map(spNodes =>
              getNodes(spNodes, "service").map(sp =>
                ServicePermission.configureFromXml(sp)))
            .flatten
            .toList
        val restrictions = UserAccessRestriction(name, servicePermissions)
        val filterFunc = (cr: CheckResult) => restrictions.permit(cr)
        val em = new ErrorMailer(name,
                                 smtp,
                                 port,
                                 username,
                                 password,
                                 xmlFromAddress) {
          override val shortcutHost = shortcutLinkServerName
          override val interestedParties: List[String] = xmlInterestedParties
          override val filterAction = (cr: CheckResult) => filterFunc(cr)
          override val initialInterval: Long = xmlInterval
          override val exponentialFactor: Int = xmlGrowthFactor
          override val maximumInterval: Long = xmlMaxInterval
          override val messagePrefix: String = xmlMessagePrefix
          override val messageSuffix: String = xmlMessageSuffix
          override val messageSubject: String = xmlMessageSubject
          override val ssl: Boolean = configSsl
        }
        em
      })
      .toList
    val newNotifiers = (diskLoggers ::: emailLoggers)
    newNotifiers.foreach(mailer => mailers = mailer :: mailers)
    newNotifiers.length match {
      case 0     => List.empty[String]
      case other => List("loaded %s notifiers".format(other))
    }
  }
  override def messageHandler = {
    case c: CheckResult => {
      mailers.foreach(_ ! c)
    }
  }
}
