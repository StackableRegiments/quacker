package metl.model

import org.apache.commons.io.IOUtils
import metl.model._
import metl.comet._
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
//Http and other utils
import com.metl.utils._
//xmpp
import org.jivesoftware._
import org.jivesoftware.smack.util.StringUtils._
import org.jivesoftware.smackx.muc.MultiUserChat
import org.jivesoftware.smack.packet.{PacketExtension, Message=>SmackMessage}
import org.jivesoftware.smack.{SASLAuthentication, ConnectionConfiguration, SmackConfiguration, XMPPConnection}
import org.jivesoftware.smack._
import org.jivesoftware.smack.filter._
import org.jivesoftware.smack.packet._
import org.jivesoftware.smackx.muc._ 
//memcached
import java.net.InetSocketAddress
import net.spy.memcached._
//svn
import org.tmatesoft.svn.core._
import org.tmatesoft.svn.core.io._
import org.tmatesoft.svn.core.auth._
import org.tmatesoft.svn.core.wc._
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl
//mysql
import java.sql.{Connection,DriverManager,SQLException,Statement,ResultSet,PreparedStatement}
//mongo
import com.mongodb._
//LDAP
import javax.naming._
import javax.naming.directory._
//PING
import java.io.{BufferedInputStream,BufferedOutputStream,File}
//TELNET
import org.apache.commons.net.telnet._
//Samba
import jcifs.smb._
import java.net.URI

case class DashboardException(reason:String,detail:String,exceptions:List[Exception] = Nil) extends Exception(reason)
case object Check
case object StartPinger
case object StopPinger

object Templates {
	//change these to vals to speed up the process
	def getStructureTemplate = TemplateFinder.findAnyTemplate(List("_Service")).openOr(NodeSeq.Empty)
	def getServiceTemplate = TemplateFinder.findAnyTemplate(List("_Check")).openOr(NodeSeq.Empty)
	def getInformationTemplate = TemplateFinder.findAnyTemplate(List("_Information")).openOr(NodeSeq.Empty)
	def getErrorInformationTemplate = TemplateFinder.findAnyTemplate(List("_ErrorInformation")).openOr(NodeSeq.Empty)
	def getEndpointInformationTemplate = TemplateFinder.findAnyTemplate(List("_EndpointInformation")).openOr(NodeSeq.Empty)
}


trait VisualElement {
	val id:String = nextFuncName
	val label:String 
	protected var internalServiceName = "unknown"
	protected var internalServerName = "unknown"
	protected def template = NodeSeq.Empty
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
	override val label = "%s information".format(name)
	override def template = Templates.getInformationTemplate
	override def templateRenderer = {
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
	override val label = name
	override def template = Templates.getInformationTemplate
	override def templateRenderer = {
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
	override val label = "Error: %s".format(name)
	override def template = Templates.getErrorInformationTemplate
	override def templateRenderer = {
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
	override val label = name
	override def template = Templates.getEndpointInformationTemplate
	override def templateRenderer = {
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

object ServiceCheckConfigurator extends ConfigFileReader {
	def configureFromXml(xml:Node, serviceName:String = "unknown", serverName:String = "unknown"):List[VisualElement] = {
		(xml \\ "serviceCheck").map(sc => {
			val periodInt = getInt(sc,"period").getOrElse(60) * 1000
			val period = new TimeSpan(periodInt)
			val label = getText(sc,"label").getOrElse("no label")
			val mode = ServiceCheckMode.parse(getText(sc,"mode").getOrElse("test"))
			val timeout = getInt(sc,"timeout").map(t => new TimeSpan(t))
			val acceptedFailures = getInt(sc,"requiredSequentialFailures")
			var failed = false
			var errors = List.empty[String]
			def getOrError[A](value:Option[A],default:A,error:String):A = value.getOrElse({
				failed = true
				errors = error :: errors
				default
			})
			val output = try {
				getText(sc,"type").map(typeString => typeString.toLowerCase.trim match {
					case "endpoint_information" => {
						val endpoints = getNodes(sc,"endpoint").map(epxml => {
							val epName = getText(epxml,"name").getOrElse("unnamed")
							val epUrl = getText(epxml,"url").getOrElse("")
							val epDesc = getText(epxml,"description").getOrElse("")
							EndpointDescriptor(epName,epUrl,epDesc)
						}).toList
						val htmlDescriptor = getText(sc,"html").getOrElse("")
						val name = getText(sc,"name").getOrElse("")
						try {
							val html = scala.xml.XML.loadString(htmlDescriptor)
							new EndpointInformationWithHtml(name,html,endpoints)
						} catch {
							case e:Throwable => 
								EndpointInformationWithString(name,htmlDescriptor,endpoints)
						}
					}
					case "information" => {
						val name = getText(sc,"name").getOrElse("")
						val htmlDescriptor = getText(sc,"html").getOrElse("")
						try {
							val html = scala.xml.XML.loadString(htmlDescriptor)
							HtmlInformation(name,html)
						} catch {
							case e:Throwable => 
								Information(name,htmlDescriptor)
						}
					}
					case "icmp" => {
						val host = getOrError(getText(sc,"host"),"","host not specified")
						val ipv6 = getBool(sc,"ipv6").getOrElse(false)
						PingICMP(mode,label,host,ipv6,period)
					}
					case "xmpp" => {
						val host = getOrError(getText(sc,"host"),"","host not specified")
						val domain = getText(sc,"domain") match {
							case Some(d) => Full(d)
							case _ => Empty
						}
						val allowAnonymousAccess = false
						PingXmpp(mode,label,host,domain,allowAnonymousAccess,period)
					}
					case "oracle" => {
						val uri = getOrError(getText(sc,"connectionUri"),"","connection string not specified")
						val username = getOrError(getText(sc,"username"),"","username not specified")
						val password = getOrError(getText(sc,"password"),"","password not specified")
						val query = getOrError(getText(sc,"query"),"","query not specified")
						val thresholds = getNodes(sc,"thresholds").map(ts => {
							val rowBehaviour = getAttr(ts,"rows").getOrElse("all")
							val tsMap = Matchers.configureFromXml(ts)
							VerifiableSqlResultSetDefinition(rowBehaviour,tsMap)
						})
						PingOracle(mode,label,uri,username,password,query,thresholds,period)
					}
					case "mysql" => {
						val host = getOrError(getText(sc,"host"),"","host not specified")
						val username = getOrError(getText(sc,"username"),"","username not specified")
						val password = getOrError(getText(sc,"password"),"","password not specified")
						val db = getOrError(getText(sc,"database"),"","database not specified")
						val query = getOrError(getText(sc,"query"),"","query not specified")
						val thresholds = getNodes(sc,"thresholds").map(ts => {
							val rowBehaviour = getAttr(ts,"rows").getOrElse("all")
							val tsMap = Matchers.configureFromXml(ts)
							VerifiableSqlResultSetDefinition(rowBehaviour,tsMap)
						})
						PingMySQL(mode,label,host,db,query,username,password,thresholds,period)
					}
					case "mongo" => {
						val host = getOrError(getText(sc,"host"),"","host not specified")
						val port = getOrError(getInt(sc,"port"),-1,"port not specified")
						val db = getOrError(getText(sc,"database"),"","database not specified")
						val table = getOrError(getText(sc,"table"),"","table not specified")
						PingMongo(mode,label,host,port,db,table,period)
					}
					case "memcached" => {
						val host = getOrError(getText(sc,"host"),"","host not specified")
						PingMemCached(mode,label,host,period)
					}
					case "ldap" => {
						val host = getOrError(getText(sc,"host"),"","host not specified")
						val username = getOrError(getText(sc,"username"),"","username not specified")
						val password = getOrError(getText(sc,"password"),"","password not specified")
						val searchTerm = getOrError(getText(sc,"searchTerm"),"","searchTerm not specified")
						val searchBase = getOrError(getText(sc,"searchBase"),"","searchTerm not specified")
						new PingLDAP(mode,label,host,username,password,searchBase,searchTerm,period)
					}
					case "munin" => {
						val host = getOrError(getText(sc,"host"),"localhost","host not specified")
						val port = getOrError(getInt(sc,"port"),4949,"port not specified")
						new PingMunin(mode,label,host,port,List(MuninCategoryDefinition("cpu",PercentageCounter),MuninCategoryDefinition("memory",Guage)),period)
					}
					case "munin_threshold" => {
						val host = getOrError(getText(sc,"host"),"localhost","host not specified")
						val port = getOrError(getInt(sc,"port"),4949,"port not specified")
						val thresholds = Map(getNodes(sc,"thresholds").map(ts => {
								val tsName = getAttr(ts,"name").getOrElse("unknown")
								val tsType = getAttr(ts,"type").getOrElse("counter")
								val tsMap = Matchers.configureFromXml(ts)
								(tsName,MuninCategoryDefinition(tsName,MuninFieldType.parse(tsType),tsMap))
							}):_*)
						PingMuninAgainstThreshhold(mode,label,host,port,thresholds,period)
					}
          case "samba" => {
            val host = getOrError(getText(sc,"host"),"localhost","host not specified")
            val domain = getOrError(getText(sc,"domain"),"workgroup","host not specified")
            val file = getOrError(getText(sc,"file"),"serverStatus","host not specified")
            val username = getOrError(getText(sc,"username"),"","host not specified")
            val password = getOrError(getText(sc,"password"),"","host not specified")
            PingSamba(mode,label,host,domain,file,username,password,period)
          }
					case "svn" => {
						val host = getOrError(getText(sc,"host"),"","host not specified")
						val username = getOrError(getText(sc,"username"),"","username not specified")
						val password = getOrError(getText(sc,"password"),"","password not specified")
						PingSVN(mode,label,host,username,password,period)
					}
					case "http" => {
						val url = getOrError(getText(sc,"url"),"","url not specified")
						val matcher:HTTPResponseMatcher = HTTPResponseMatchers.configureFromXml(<thresholdsPacket>{getNodes(sc,"thresholds")}</thresholdsPacket>)
						val additionalHeaders = getNodes(sc,"header").map(c => {
							(getText(c,"name").getOrElse(""),getText(c,"value").getOrElse(""))
						}).filter(h => {
							h match {
								case (n:String,v:String) if n.length > 0 && v.length > 0 => true
								case _ => false
							}
						}).toList
						HttpCheck(mode,label,url,additionalHeaders,matcher,period)
					}
					case "http_with_credentials" => {
						val url = getOrError(getText(sc,"url"),"","url not specified")
						val username = getOrError(getText(sc,"username"),"","username not specified")
						val password = getOrError(getText(sc,"password"),"","password not specified")
						val matcher:HTTPResponseMatcher = HTTPResponseMatchers.configureFromXml(<thresholdsPacket>{getNodes(sc,"thresholds")}</thresholdsPacket>)
						val additionalHeaders = getNodes(sc,"header").map(c => {
							(getText(c,"name").getOrElse(""),getText(c,"value").getOrElse(""))
						}).filter(h => {
							h match {
								case (n:String,v:String) if n.length > 0 && v.length > 0 => true
								case _ => false
							}
						}).toList
						HttpCheckWithBasicAuth(mode,label,url,username,password,additionalHeaders,matcher,period)
					}
 					case "dependency" => {
						val matchers:Map[DependencyDescription,DependencyMatcher] = Map(getNodes(sc,"thresholds").map(t => {
							val pinger = getAttr(t,"check").getOrElse("unknown")
							val service = getAttr(t,"service")
							val server = getAttr(t,"server")
							val serviceCheckMode = getAttr(t,"serviceCheckMode").map(scm => ServiceCheckMode.parse(scm))
							val desc = DependencyDescription(pinger,server,service,serviceCheckMode)
							val matcher = DependencyMatchers.configureFromXml(t)
							(desc,matcher)
						}):_*)
						DependencyCheck(mode,label,matchers,period)
					}
					case "matcher" => {
            getImmediateNodes(sc,"matcher").map(mNodes => {
              Matchers.configureFromXml(mNodes).headOption.map(ho => {
                val matcher = ho._2
                MatcherCheck(mode,label,matcher,period)
              }).getOrElse(NullCheck)
            }).headOption.getOrElse(NullCheck)
					}
          case "script" => {
            val interpolator = Interpolator.configureFromXml( <interpolators>{getImmediateNodes(sc,"interpolator")}</interpolators> )
            val sequence = FunctionalCheck.configureFromXml( <steps>{getImmediateNodes(sc,"step")}</steps> )
            ScriptedCheck(mode,label,sequence,interpolator.getOrElse(EmptyInterpolator),period)  
          }
					case other => {
						failed = true
						errors = "unknown type" :: errors
						NullCheck
					}
				}).map{
					case p:Pinger => {
						timeout.map(t => p.checkTimeout = Full(t))
						acceptedFailures.map(ft => p.failureTolerance = ft)
						p.serverName(serverName)
						p.serviceName(serviceName)
						p
					}
					case other => {
						other.serverName(serverName)
						other.serviceName(serviceName)
						other
					}
				}.getOrElse({
					failed = true
					errors = "no type specified" :: errors
					NullCheck
				})
			} catch {
				case e:Throwable => {
					errors = e.getMessage :: errors
					NullCheck
				}
			}
			if (failed)
				ErrorInformation("serviceCheck couldn't be constructed from xml",period.toString,sc.toString,errors)
			else output 
		}).toList
	}
}

abstract class Pinger(incomingLabel:String, incomingMode:ServiceCheckMode) extends LiftActor with VisualElement with metl.comet.CheckRenderHelper with Logger {
  import GraphableData._
	val mode = incomingMode
	override val label = incomingLabel
	override def template = Templates.getServiceTemplate
	var checkTimeout:Box[TimeSpan] = Empty
	var failureTolerance = 1
	var lastCheckBegin:Box[Date] = Empty
	var lastUptime:Box[Date] = Empty
	def lastUptimeString = lastUptime.map(t => t.toString).openOr("NEVER")
	var lastCheck:Box[Date] = Empty
	var lastStatus:Box[Boolean] = Empty
	var lastWhy:Box[String] = Empty
	var lastDetail:Box[String] = Empty
	var currentFailures = 0
	def lastStatusClass = statusClassFromStatus(lastStatus)
	def lastStatusCode = statusCodeFromStatus(lastStatus)
  protected val pollInterval = 5 seconds
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
    ActorPing.schedule(this,Check,interval)
  }
  def fail(why:String,detail:String = "") = {
		val lastUp = lastUptime
		val now = updatedTime(false)
		lastStatus = Full(false)
		currentFailures = currentFailures + 1
    val cr = CheckResult(id,label,getServiceName,getServerName,now,why,lastUp,detail,mode,false)
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
    var cr =  CheckResult(id,label,getServiceName,getServerName,now,why,lastUp,"",mode,true,data)
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
    List(
      JField("type",JString("pinger")),
      JField("lastChecked",JString(lastCheck.map(d => d.toString).openOr("NEVER"))),
      JField("lastSuccess",JBool(lastStatus.openOr(false))),
      JField("lastStatusCode",JString(lastStatusCode)),
      JField("lastUp",JString(lastUptimeString)),
      JField("mode",JString(mode.toString)),
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
      ("mode",Str(mode.toString)),
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

case class PingXmpp(serviceCheckMode:ServiceCheckMode, incomingLabel:String,resource:String,serviceName:Option[String]=None,allowAnonymousAccess:Boolean=false,time:TimeSpan = 10 seconds) extends Pinger(incomingLabel,serviceCheckMode){
	val hostname = resource
	val service = serviceName.getOrElse(hostname)
  override val pollInterval = time
  XMPPConnection.DEBUG_ENABLED = false
  SmackConfiguration.setPacketReplyTimeout(15000)
	SmackConfiguration.setKeepAliveInterval(5000)
	SASLAuthentication.supportSASLMechanism("PLAIN", 0)
	def createConfig = {
		val config = new ConnectionConfiguration(hostname,5222,service)
		config.setCompressionEnabled(false)
		config.setSendPresence(false)
		config.setRosterLoadedAtLogin(false)
		config.setSelfSignedCertificateEnabled(true)
		config.setSASLAuthenticationEnabled(true)
		config.setReconnectionAllowed(false)
		config.setExpiredCertificatesCheckEnabled(false)
		config.setNotMatchingDomainCheckEnabled(false)
		config.setVerifyChainEnabled(false)
		config.setVerifyRootCAEnabled(false)
		config.setDebuggerEnabled(false)
		config
	}
	private val smackConfig = createConfig
	def createConnection = new XMPPConnection(smackConfig)
  var conn = createConnection
  override protected def exceptionHandler = ({
    case expected:Throwable if expected.toString.contains("SASL authentication failed") => {
      val exceptionMessage = "Xmpp server responded correctly: %s".format(expected.toString)
			succeed(exceptionMessage)
			schedule()	
    }
    case other:Throwable => {
      fail(other.toString)
      schedule()
    }
  }:PartialFunction[Throwable,Unit]) orElse super.exceptionHandler
  override def resetEnvironment = {
    if (conn != null && conn.isConnected)
      conn.disconnect
    conn = createConnection
  }
  def status = {
    if (conn != null && conn.isConnected)
      conn.disconnect
    conn.connect
		if (conn.isConnected){
    	conn.login("anillegaluser@".format(service),"shouldnotbeaccepted", new java.util.Date().getTime.toString)
			conn.disconnect
			if (!allowAnonymousAccess)
				throw new DashboardException("XMPP behaved inappropriately","XMPP allowed this user in, and shouldn't have")
			else 
				"login succeeded"
		} else {
			throw new DashboardException("XMPP behaved inappropriately","Xmpp connection to %s didn't successfully connect".format(hostname))
		}
  }
	override def performCheck = {
		succeed(status.toString)
	}
}

class MSMap[A,B](defaultFunc:A=>B = (a:A) => null.asInstanceOf[B]) extends scala.collection.mutable.HashMap[A,B] with scala.collection.mutable.SynchronizedMap[A,B]{
	override def default(key:A):B = defaultFunc(key)
}
abstract class MuninFieldType {}
object MuninFieldType {
	def parse(input:String):MuninFieldType = input.toLowerCase.trim match {
		case "counter" => Counter
		case "guage" => Guage
		case "percentagecounter" => PercentageCounter
		case "counteraspercentage" => PercentageCounter
		case "percentageguage" => PercentageGuage
		case "guageaspercentage" => PercentageGuage
		case _ => Counter
	}
}
case object Counter extends MuninFieldType
case object Guage extends MuninFieldType
case object PercentageCounter extends MuninFieldType
case object PercentageGuage extends MuninFieldType

case class MuninCategoryDefinition(name:String,fieldType:MuninFieldType,matchers:Map[String,Matcher] = Map.empty[String,Matcher])

class PingMunin(serviceCheckMode:ServiceCheckMode, incomingLabel:String, host:String, port:Int, onlyFetch:List[MuninCategoryDefinition] = List(MuninCategoryDefinition("cpu",Counter),MuninCategoryDefinition("memory",Guage)), time:TimeSpan = 5 seconds) extends PingTelnet(serviceCheckMode,incomingLabel,host,port,time){
	protected val previous = {
		val map = new MSMap[String,MSMap[String,Double]]()
		onlyFetch.map(munCatDef => map.put(munCatDef.name,new MSMap[String,scala.Double]()))
		map
	}
	override val commandResponseTerminator:Option[String] = Some("\n.\n")
	protected def generatedDelta[Double](inputName:String,input:Map[String,scala.Double]):Map[String,scala.Double] = {
		val result = previous.get(inputName).map(po => Map(input.keys.map(ink => {
			val updatedValue = (po(ink),input(ink)) match {
				case (p:scala.Double,i:scala.Double) if (i < p) => {
					// this is the counter reset behaviour.  I do believe that counters occasionally reset themselves to zero in munin
					debug("possibleOverflow: %s (%s -> %s)".format(ink,p,i))
					val out = i
					po.put(ink,0.0)
					out
				}
				case (p:scala.Double,i:scala.Double) => {
					debug("correct counter behaviour: %s (%s -> %s)".format(ink,p,i))
					val out = i - p
					po.put(ink,i)
					out
				}
				case other => {
					debug("resetting to zero: %s (%s)".format(ink,other))
					val out = 0.0
					po.put(ink,0.0)
					out
				}
			}
			(ink,updatedValue)
		}).toList:_*)).getOrElse(input)
		result
	}
	protected def interpretMuninData(tc:TelnetClient):Map[String,Map[String,Double]] = {
		val outputStream = new BufferedOutputStream(tc.getOutputStream)
		val inputStream = new BufferedInputStream(tc.getInputStream)
		var output = readStream(inputStream)
		if (output.length == 0)
			throw new DashboardException("Munin failed","no response from remote node")
		writeTo("list",outputStream)
		val possibleQueries = readStream(inputStream).split(" ").toList
		val desiredQueries = onlyFetch.filter(of => possibleQueries.contains(of.name))
		val completeOutput = Map(desiredQueries.map(of => {
			val o = of.name
			writeTo("fetch "+o,outputStream)
			val individualOutput = readStream(inputStream,commandResponseTerminator)
			val formattedOutput = Map(individualOutput.split("\n").filter(l => l.length > 1).map(l => {
				val parts = l.split(" ")
				val muninDataKey = parts(0).reverse.dropWhile(c => c != '.').drop(1).reverse.toString
				val muninDataValue = tryo(parts(1).toDouble).openOr(-1.0)
				(muninDataKey,muninDataValue)
			}):_*)
			val finalOutput = of.fieldType match {
				case Counter => {
					try {
						val deltas = generatedDelta(o, formattedOutput)
						Map(deltas.toList.map(fo => (fo._1,fo._2.toDouble)):_*)
					} catch {	
						case _ => formattedOutput
					}
				}
				case Guage => {
					formattedOutput
				}
				case PercentageCounter => {
					try {
						val deltas = generatedDelta(o,formattedOutput)
						val total = deltas.values.sum
						val deltaPercentages = Map(deltas.toList.map(d => (d._1, ((d._2 / total) * 100))):_*)
						deltaPercentages
					} catch {
						case _ => formattedOutput
					}
				}
				case PercentageGuage => {
					try {
						val total = formattedOutput.values.sum
						Map(formattedOutput.toList.map(fo => (fo._1,((fo._2 / total) * 100))):_*)
					} catch {
						case _ => formattedOutput
					}
				}
			}
			(o,finalOutput)	
		}):_*)
		writeTo("quit",outputStream)
		outputStream.close
		inputStream.close
		completeOutput
	}
	override def telnetBehaviour(tc:TelnetClient):List[String] = {
		val completeOutput = interpretMuninData(tc)
		//inform(true,previous.toMap)
		completeOutput.keys.map(cok => "%s -> %s".format(cok,completeOutput(cok))).toList
	}
}

case class PingMuninAgainstThreshhold(serviceCheckMode:ServiceCheckMode, incomingLabel:String, host:String, port:Int, thresholds:Map[String,MuninCategoryDefinition] = Map.empty[String,MuninCategoryDefinition], time:TimeSpan = 5 seconds) extends PingMunin(serviceCheckMode,incomingLabel,host,port,thresholds.values.toList,time){
	override def telnetBehaviour(tc:TelnetClient):List[String] = {
		val completeOutput = interpretMuninData(tc)
		var errors = List.empty[String]
		def addError(newError:String):Unit = { 
			errors = errors ::: List(newError)
		}
		thresholds.keys.map(thresholdsKey => {
			val muninCatDef = thresholds(thresholdsKey)
			muninCatDef.matchers.keys.foreach(matcherKey => {
				completeOutput.get(thresholdsKey).map(completeOutputValueMap => {
					completeOutputValueMap.get(matcherKey).map(completeOutputValue => {
						val thresholdVerifier = muninCatDef.matchers(matcherKey)
						if (!thresholdVerifier.verify(completeOutputValue)){
							addError("%s.%s (%s) (%s)".format(thresholdsKey,matcherKey,completeOutputValue,thresholdVerifier.describe))
							false
						} else {
							true
						}
					}).getOrElse({
							addError("munin didn't return %s.%s".format(thresholdsKey,matcherKey))
							false
					})
				}).getOrElse({
						addError("munin didn't return %s".format(thresholdsKey))
						false
				})
			})
		})
		if (errors.length > 0){
			//inform(false,previous.toMap)
			throw new DashboardException("Munin failed to meet a specified threshold",errors.mkString("\r\n"))
		}
		//inform(true,previous.toMap)
		completeOutput.keys.map(cok => "%s -> %s".format(cok,completeOutput(cok))).toList
	}
}

class PingTelnet(serviceCheckMode:ServiceCheckMode, incomingLabel:String,host:String,port:Int,time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
	protected val commandResponseTerminator:Option[String] = None
	protected def telnetBehaviour(tc:TelnetClient):List[String] = {
		val inputStream = new BufferedInputStream(tc.getInputStream)
		val output = readStream(inputStream)	
		inputStream.close
		output.split("\n").toList
	}
	protected def writeTo(input:String,stream:BufferedOutputStream) = {
		val command = (input + "\n").getBytes("ISO-8859-1")
		stream.write(command,0,command.length)
		stream.flush	
	}
	protected def readStream(input:BufferedInputStream,endPattern:Option[String] = None):String = readStreamLoop(input,endPattern,new Date(),5000L)
	protected def readStreamLoop(input:BufferedInputStream,endPattern:Option[String],startTime:Date, timeout:Long):String = {
		var output = ""
		var hasFinished = false
		while (input.available > 0 && !hasFinished){
			output += input.read.asInstanceOf[Char]
			endPattern.map(ep => {
				hasFinished = output.endsWith(ep)
			})
		}
		if (output == "" && (new Date().getTime - startTime.getTime) < timeout){
			Thread.sleep(timeout / 10)	
			output += readStreamLoop(input,endPattern,startTime,timeout)
		}
		output
	}
  def status = { 
		val tc = new TelnetClient()
		tc.connect(host,port)
		val output = telnetBehaviour(tc)
		tc.disconnect
		output.mkString("\n")
  }
	override def performCheck = succeed(status)
}


case class PingICMP(serviceCheckMode:ServiceCheckMode, incomingLabel:String,uri:String,ipv6:Boolean = false,time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
		//pinging is done via ipv4 at present.  ipv6 in some networks results in unexpected results for some subnets
   private val pingCmd = (ServiceConfigurator.isWindows,ServiceConfigurator.isLinux,ServiceConfigurator.isOSX,ipv6) match {
		case (true,false,false,false) => "ping -4 -n 1 "+uri
		case (true,false,false,true) => "ping -6 -n 1 "+uri
		case (false,true,false,false) => "ping -c 1 "+uri
		case (false,true,false,true) => "ping6 -c 1 "+uri
		case (false,false,true,false) => "ping -c 1 "+uri
		case (false,false,true,true) => "ping6 -c 1 "+uri
		case _ => "ping -c 1 "+uri
	 }	
	 private val pingTimeExtractor:String=>Box[Double] = (ServiceConfigurator.isWindows,ServiceConfigurator.isLinux,ServiceConfigurator.isOSX) match {
		case (true,false,false) => (output:String) => {
			val reg = """time([=<])([0-9]+)ms""".r
			reg.findFirstMatchIn(output) match {
				case Some(timeMatch) => {
					val timeString = timeMatch.group(2)
					timeMatch.group(1) match {
						case "=" => tryo(timeString.toLong)
					//approximating time to 0.95 of time, if the ping response read "time<1ms" instead of "time=1ms"
						case "<" => tryo(timeString.toLong * 0.95)
					}
				}
				case _ => Empty
			}
		}
		case (false,true,false) => (output:String) => {
			val reg = """time[=<]([0-9.]+) ms""".r
			reg.findFirstMatchIn(output) match {
				case Some(timeMatch) => {
					val timeString = timeMatch.group(1)
					tryo(timeString.toDouble)
				}
				case _ => Empty
			}
		}
		case (false,false,true) => (output:String) => {
			val reg = """time[=<]([0-9.]+) ms""".r
			reg.findFirstMatchIn(output) match {
				case Some(timeMatch) => {
					val timeString = timeMatch.group(1)
					tryo(timeString.toDouble)
				}
				case _ => Empty
			}
		}
		case _ => (output:String) => Empty
	 }	

   override val pollInterval = time
	 failureTolerance = 3
   def status = { 
      val pingProcess = Runtime.getRuntime().exec(pingCmd)
      val inputStream = new BufferedInputStream(pingProcess.getInputStream)
      val errorStream = new BufferedInputStream(pingProcess.getErrorStream)
      var output = ""
      pingProcess.waitFor
      while (errorStream.available > 0 || inputStream.available > 0 )
      {
        while (inputStream.available > 0)
          output += inputStream.read.asInstanceOf[Char]
        while (errorStream.available > 0)
          output += errorStream.read.asInstanceOf[Char]
      }
      pingProcess.destroy
      if (output.length == 0)
        throw new DashboardException("Ping failed","ping command failed - no response from OS")
      if (output.contains("cannot resolve") || output.contains("Unknown host") || output.contains("could not find host"))
        throw new DashboardException("Ping failed","Unknown host: "+output)
			if (!(output.contains(" 0% packet loss") || output.contains("(0% loss)")))
				throw new DashboardException("Ping failed","Packet loss recognised: "+output)
			val stringOutput = output.toString
			val timeTaken = pingTimeExtractor(stringOutput)
			(stringOutput,timeTaken)
  }
	override def performCheck = succeed(status._1,status._2)
}

case class SQLResultSet(rows:Map[Int,SQLRow]){
	def verify(matchers:Map[String,Matcher]):Boolean = {
		!(rows.values.map(r => r.verify(matchers)).exists(m => !m))
	}
}
case class SQLRow(rowNumber:Int,cells:Map[String,SQLCell[Any]]){
	def verify(matchers:Map[String,Matcher]):Boolean = {
		!(matchers.keys.map(mk => matchers(mk).verify(cells(mk))).exists(m => !m))
	}
}
case class SQLCell[A](name:String,value:A){
	def verify(matcher:Matcher):Boolean = {
		matcher.verify(value)
	}
}

object VerifiableSQLResultSetConverter {
	def toVerifiableSQLResultSet(resultSet:java.sql.ResultSet):SQLResultSet = {
		val metaData = resultSet.getMetaData
		var rowNumber = 1
		if (resultSet.isBeforeFirst()){
			resultSet.next()
		}
		var output = List(toRow(rowNumber,resultSet,Some(metaData)))
		while (resultSet.next()){
			rowNumber = rowNumber + 1
			output = output ::: List(toRow(rowNumber,resultSet,Some(metaData)))
		}
		SQLResultSet(Map(output.map(o => (o.rowNumber,o)):_*))
	}
	protected def toRow(rowNumber:Int,resultSet:java.sql.ResultSet, metaData:Option[java.sql.ResultSetMetaData] = None):SQLRow = {
		val md = metaData.getOrElse(resultSet.getMetaData)
		val output = Map(Range(1,md.getColumnCount() + 1).map(colNumber => {
			val colType = md.getColumnType(colNumber)
			val name = md.getColumnName(colNumber)
			val internalOutput = colType match {
				case /*ARRAY*/ 2003 => SQLCell(name,"not deconstructing ARRAYs")
				case /*BIGINT*/ -5 => SQLCell(name,resultSet.getLong(colNumber))
				case /*BINARY*/ -2 => SQLCell(name,resultSet.getBytes(colNumber))
				case /*BIT*/ -7 => SQLCell(name,resultSet.getInt(colNumber))
				case /*BLOB*/ 2004 => {
					val blob = resultSet.getBlob(colNumber)
					SQLCell(name,blob.getBytes(0,blob.length.toInt))
				}
				case /*BOOLEAN*/ 16 => SQLCell(name,resultSet.getBoolean(colNumber))
				case /*CHAR*/ 1 => SQLCell(name,resultSet.getByte(colNumber))
				case /*CLOB*/ 2005 => SQLCell(name,resultSet.getString(colNumber))
				case /*DATALINK*/ 70 => SQLCell(name,"not deconstructing DATALINKs")
				case /*DATE*/ 91 => SQLCell(name,resultSet.getDate(colNumber).toString)
				case /*DECIMAL*/ 3 => SQLCell(name,resultSet.getBigDecimal(colNumber).toString)
				case /*DISTINCT*/ 2001 => SQLCell(name,"not deconstructing DISTINCTs")
				case /*DOUBLE*/ 8 => SQLCell(name,resultSet.getDouble(colNumber))
				case /*FLOAT*/ 6 => SQLCell(name,resultSet.getFloat(colNumber))
				case /*INTEGER*/ 4 => SQLCell(name,resultSet.getInt(colNumber))
				case /*JAVA_OBJECT*/ 2000 => SQLCell(name,resultSet.getObject(colNumber))
				case /*LONGVARBINARY*/ -4 => SQLCell(name,"not deconstructing LONGVARBINARYs")
				case /*LONGVARCHAR*/ -1 => SQLCell(name,"not deconstructing LONGVARCHARs")
				case /*LONGNVARCHAR*/ -16 => SQLCell(name,"not deconstructing LONGNVARCHARs")
				case /*NCHAR*/ -15 => SQLCell(name,"not deconstructing NCHARs")
				case /*NCLOB*/ 2011 => SQLCell(name,"not deconstructing NCLOBs")
				case /*NULL*/ 0 => SQLCell(name,"SQL NULL returned")
				case /*NUMERIC*/ 2 => SQLCell(name,resultSet.getLong(colNumber))
				case /*NVARCHAR*/ -9 => SQLCell(name,"not deconstructing NVARCHARs")
				case /*OTHER*/ 1111 => SQLCell(name,"not deconstructing OTHERs")
				case /*REAL*/ 7 => SQLCell(name,resultSet.getLong(colNumber))
				case /*REF*/ 2006 => SQLCell(name,"not deconstructing REFs")
				case /*ROWID*/ -8 => SQLCell(name,resultSet.getRowId(colNumber).toString)
				case /*SMALLINT*/ 5 => SQLCell(name,resultSet.getInt(colNumber))
				case /*SQLXML*/ 2009 => SQLCell(name,"not deconstructing SQLXMLs")
				case /*STRUCT*/ 2002 => SQLCell(name,"not deconstructing STRUCTs")
				case /*TIME*/ 92 => SQLCell(name,"not deconstructing TIMEs")
				case /*TIMESTAMP*/ 93 => SQLCell(name,"not deconstructing TIMESTAMPs")
				case /*TINYINT*/ -6 => SQLCell(name,resultSet.getInt(colNumber))
				case /*VARBINARY*/ -3 => SQLCell(name,"not deconstructing VARBINARYs")
				case /*VARCHAR*/ 12 => SQLCell(name,resultSet.getString(colNumber))
				case other => SQLCell(name,"not deconstructing UNKNOWNs (%s)".format(other))
			}
			(name,internalOutput)
		}):_*)
		SQLRow(rowNumber,output.asInstanceOf[Map[String,SQLCell[Any]]])
	}
}

case class VerifiableSqlResultSetDefinition(rowBehaviour:String,matchers:Map[String,Matcher]){
	val internalRowBehaviour = rowBehaviour.toLowerCase.trim match {
		case "all" => "all"
		case n:String => {
			try {
				n.toInt.toString
			} catch {
				case e:Throwable => "none"
			}	
		}
		case other => "none"
	}
	val verifyRowFunc:SQLRow=>Boolean = {
		internalRowBehaviour match {
			case "all" => (a:SQLRow) => true
			case "none" => (a:SQLRow) => false
			case "any" => (a:SQLRow) => true
			case n:String => {
				try {
					val rowNumber = n.toInt
					(a:SQLRow) => rowNumber == a.rowNumber 
				} catch {
					case e:Throwable => (a:SQLRow) => false
				}
			}
			case _ => (a:SQLRow) => false
		}
	}
	def verifyResultSet(resultSet:SQLResultSet):VerificationResponse = {
		var errors = List.empty[String]
		var successes = List.empty[SQLRow]
		resultSet.rows.toList.foreach(rsr => {
			val resultSetRow = rsr._2
			val rowNumber = rsr._1
			if (verifyRowFunc(resultSetRow)){
				matchers.keys.foreach(mk => {
					val pertinentCell = resultSetRow.cells(mk)
					val matcher = matchers(mk)
					if (!pertinentCell.verify(matcher)){
						errors = errors ::: List("row (%s) (%s) failed verification from row(%s) %s".format(rowNumber, resultSetRow, internalRowBehaviour, matcher.describe))
					} else {
						successes = successes ::: List(resultSetRow)
					}
				})
			}
		})
		internalRowBehaviour match {
			case "all" => VerificationResponse(resultSet.rows.toList.length > 0 && errors.length == 0,errors)
			case "none" => VerificationResponse(resultSet.rows.toList.length > 0 && successes.length == 0,errors)
			case "some" => VerificationResponse(resultSet.rows.toList.length > 0 && successes.length > 0,errors)
			case "someNot" => VerificationResponse(resultSet.rows.toList.length > 0 && errors.length > 0,errors)
			case other => VerificationResponse(resultSet.rows.toList.length > 0 && errors.length == 0,errors)
		}
	}
}

object OracleSetup {
  Class.forName("oracle.jdbc.OracleDriver").newInstance()
	def initialize = {}
}

class CombinedExceptionsException(exceptions:List[Throwable]) extends DashboardException("multiple exceptions thrown",exceptions.map(e => e.getMessage).mkString("\r\n"),exceptions.flatMap(_ match {
  case e:Exception => Some(e)
  case _ => None
}))

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeoutException
import scala.concurrent.{Future,Await}

case class PingOracle(serviceCheckMode:ServiceCheckMode, incomingLabel:String,uri:String,username:String,password:String, query:String, thresholds:List[VerifiableSqlResultSetDefinition] = List.empty[VerifiableSqlResultSetDefinition], time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
	OracleSetup.initialize
	protected val connectionCreationTimeout = 10000L
	override def performCheck = {
		var output = SQLResultSet(Map.empty[Int,SQLRow])
		var errors = List.empty[Throwable]
		try {
      Await.result(Future(Some({
				val result = try {
					val conn = DriverManager.getConnection("jdbc:oracle:thin:@%s".format(uri),username,password)
					val statement = conn.createStatement
					var failedVerificationResponses = List.empty[VerificationResponse]
					val resultSet = statement.executeQuery(query)
					output = VerifiableSQLResultSetConverter.toVerifiableSQLResultSet(resultSet)
					resultSet.close
					conn.close
					val verificationResponses = thresholds.map(t => t.verifyResultSet(output))
					failedVerificationResponses = verificationResponses.filter(pfv => !pfv.success)
					if (failedVerificationResponses.length > 0){
						errors = errors ::: List(new DashboardException("SQL Verification failed",failedVerificationResponses.map(fvr => fvr.errors).flatten.mkString("\r\n")))
						(false,conn) 
					} else {
						succeed(output.toString)
						(true,conn)
					}
				} catch {
					case e:Throwable => {
						errors = errors ::: List(e)
						(false,null.asInstanceOf[java.sql.Connection])	
					}
				}
        result match {
          case l:List[Option[Option[Tuple2[Boolean,Connection]]]] if l.length > 0 => l.head match {
            case Some(Some((_,null))) => {
              errors = errors ::: List(new DashboardException("SQL Connection failed","connection is null"))
            }
            case Some(Some((true,connection))) => {
            }
            case Some(Some((false,other))) => {
              errors = errors ::: List(new DashboardException("SQL Connection failed","connection: %s".format(other.toString)))
            }
            case other => {
              errors = errors ::: List(new DashboardException("SQL Connection failed","other: %s".format(other.toString)))
            }
          }
        }
			})),Duration(connectionCreationTimeout,"millis")) 
    } catch {
      case e:TimeoutException => {
        errors = errors ::: List(new DashboardException("SQL Connection failed","oracle failed to create a connection within the timeout",List(e)))
      }
    }
		if (errors.length == 1) {
			throw errors.head
		} else if (errors.length > 0) {
			throw new CombinedExceptionsException(errors)
		} 
  }
}

object MySQLSetup {
  Class.forName("com.mysql.jdbc.Driver").newInstance()
	def initialize = {}	
}

case class PingMySQL(serviceCheckMode:ServiceCheckMode, incomingLabel:String,uri:String,database:String,query:String,username:String,password:String,thresholds:List[VerifiableSqlResultSetDefinition] = List.empty[VerifiableSqlResultSetDefinition], time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
	MySQLSetup.initialize
  var sqlConnection:Option[Connection] = None
 	protected val connectionCreationTimeout = 3000L
  override def resetEnvironment = {
    sqlConnection.map(_.close)
    sqlConnection = try {
      Await.result(Future(Some(DriverManager.getConnection("jdbc:mysql://%s/%s".format(uri,database),username,password))),Duration(connectionCreationTimeout,"millis"))
    } catch {
      case e:TimeoutException => {
        error("mysql failed to create a connection within the timeout",e)
        None
      }
    }
  }
	def status = {
			resetEnvironment
      val statementOption = sqlConnection.map(_.createStatement)
			var output = SQLResultSet(Map.empty[Int,SQLRow])
			var failedVerificationResponses = List.empty[VerificationResponse]
			statementOption.map(statement => {
				val resultSet = statement.executeQuery(query)
				output = VerifiableSQLResultSetConverter.toVerifiableSQLResultSet(resultSet)
				resultSet.close
				val verificationResponses = thresholds.map(t => t.verifyResultSet(output))
				failedVerificationResponses = verificationResponses.filter(pfv => !pfv.success)
				output
			}).getOrElse(throw new DashboardException("unable to connect to database",""))
      sqlConnection.map(_.close)
			if (failedVerificationResponses.length > 0){
				throw new DashboardException("SQL Verification failed",failedVerificationResponses.map(fvr => fvr.errors).flatten.mkString("\r\n"))
			}
			output
  }
	override def performCheck = succeed(status.toString)
}

case class PingSamba(serviceCheckMode:ServiceCheckMode, incomingLabel:String,hostname:String,domain:String,filename:String,username:String,password:String,time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode) {
  override val pollInterval = time
  def isForURI(uri:URI):Boolean = uri.getHost == hostname && uri.getScheme == "smb"
  val auth = new NtlmPasswordAuthentication(domain,username,password)
  val path = filename match {
        case fp if fp.startsWith("smb://") && isForURI(new URI(fp)) => fp
        case fp if fp.startsWith("smb://") => throw new Exception("path requested which is not for this connection")
        case p => "smb://%s/%s".format(hostname,p)
      }
  def status = {
    val file = new SmbFile(path,auth)
    if (file.isFile){
      "File exists: %s, size: %s, lastModified: %s".format(file.exists,file.getContentLength,file.lastModified)
    } else {
      val fileList = file.list() match {
        case a:Array[String] if a.length == 0 => throw new DashboardException("Samba exception","resource empty")
        case a:Array[String] => a.toList
        case null => throw new DashboardException("Samba exception","resource cannot be resolved")
        case other => throw new DashboardException("Samba exception","unexpected response from SmbFile.list => %s".format(other))
      }
      fileList.mkString(", ")
    }
  }
	override def performCheck = succeed(status.toString)
}

case class PingMongo(serviceCheckMode:ServiceCheckMode, incomingLabel:String,hostname:String,port:Int,database:String,table:String,time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
	var mongo = new Mongo(hostname,port)
  def status = {
      mongo.close
      mongo = new Mongo(hostname,port)
			val db = mongo.getDB(database)
			val collection = db.getCollection(table)
			val output = collection.findOne.toMap
			mongo.close
			output
  }
	override def performCheck = succeed(status.toString)
}
case class PingMemCached(serviceCheckMode:ServiceCheckMode, incomingLabel:String,uri:String, time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
  private	val port = 11211
	private val address = new InetSocketAddress(uri,11211)
  private var cache = new MemcachedClient(address)
  def status = {
		cache.shutdown
    cache = new MemcachedClient(address)
		val stats = cache.getStats
		cache.shutdown
		stats
  }
  override protected def exceptionHandler = ({
    case expected:java.util.ConcurrentModificationException => {
      val exceptionMessage = "Memcached threw a non-critical exception: %s".format(expected.toString)
			succeed(exceptionMessage)	
			schedule()	
    }
    case other:Throwable => {
      fail(other.toString)
      schedule()
    }
  }:PartialFunction[Throwable,Unit]) orElse super.exceptionHandler
	override def performCheck = succeed(status.toString)
}
class PingLDAP(serviceCheckMode:ServiceCheckMode, incomingLabel:String,host:String,username:String,password:String,searchBase:String,searchTerm:String,time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
	private val infoGroups = Seq("cn","uid","sn","givenname","mail")
	private val env = new java.util.Hashtable[String,String]();
	env.put(Context.INITIAL_CONTEXT_FACTORY,"com.sun.jndi.ldap.LdapCtxFactory")
                	env.put(Context.PROVIDER_URL,host)
                	env.put(Context.SECURITY_AUTHENTICATION,"simple")
                	env.put(Context.SECURITY_PRINCIPAL,username)
                	env.put(Context.SECURITY_CREDENTIALS,password)
  private var ctx = new InitialDirContext(env)
  private val controls = new SearchControls
  controls.setSearchScope(SearchControls.SUBTREE_SCOPE)
  def status = {
      ctx.close
      ctx = new InitialDirContext(env)
      val results = ctx.search(searchBase,searchTerm,controls)
      val output = results.hasMore match {
        case true => {
          val rawAttributes = results.next().getAttributes()
          infoGroups.map(
            group => (group,rawAttributes.get(group))
            ).filter{
              case (name,attr) => attr != null
            }.map{
              case (name:String,attrib:javax.naming.directory.Attribute) => {
                attrib.getAll match {
                  case namingEnum:NamingEnumeration[_] => {
                    var mySeq = List.empty[(String,String)]
                    while(namingEnum.hasMore())
                      mySeq = (name,namingEnum.next.toString) :: mySeq
                    mySeq
                  } 
                } 
              }
              case _ => ""
            }.toString
          }
        case false => "user (%s) not found in HDS".format(searchTerm)	
      }
      results.close
      ctx.close
			output
  }
	override def performCheck = succeed(status.toString)
}

case class PingSVN(serviceCheckMode:ServiceCheckMode, incomingLabel:String,server:String,username:String,password:String,time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
  SVNRepositoryFactoryImpl.setup
  private val authManager = SVNWCUtil.createDefaultAuthenticationManager(username,password)
  private var repo = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(server),null)
  def status = {
    repo.closeSession
    repo = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(server),null)
    repo.setAuthenticationManager(authManager)
    val logEntries = repo.log(Array.empty[String], null, -1, -1, true, true)
    repo.closeSession
	  logEntries.toString
  }
	override def performCheck = succeed(status.toString)
}
case class CheckUnexceptional(serviceCheckMode:ServiceCheckMode, incomingLabel:String,condition:Function0[Any],time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
  def status = condition()
	override def performCheck = succeed("Was expected")
}
case class CheckDoesnt(serviceCheckMode:ServiceCheckMode, incomingLabel:String,condition:Function0[Option[String]], time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
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
object HTTPResponseMatchers extends ConfigFileReader {
	def configureFromXml(n:Node):HTTPResponseMatcher = {
		val matcher = new HTTPResponseMatcher	
		getNodes(n,"matcher").map(mn => {
			getAttr(mn,"name").getOrElse("unknown") match {
				case "response" => matcher.setResponseVerifier(Matchers.configureVerificationFuncFromXml(mn))
				case "requestUrl" => matcher.setRequestUrlVerifier(Matchers.configureVerificationFuncFromXml(mn))
				case "duration" => matcher.setDurationVerifier(Matchers.configureVerificationFuncFromXml(mn))
				case "statusCode" => matcher.setCodeVerifier(Matchers.configureVerificationFuncFromXml(mn))
				case "headers" => matcher.setHeadersVerifier(Matchers.configureVerificationFuncFromXml(mn))
				case "retries" => matcher.setRetriesVerifier(Matchers.configureVerificationFuncFromXml(mn))
				case "redirects" => matcher.setRedirectsVerifier(Matchers.configureVerificationFuncFromXml(mn))
				case "exceptions" => matcher.setExceptionsVerifier(Matchers.configureVerificationFuncFromXml(mn))
				case _ => {}
			}
		})
		matcher
	}
	object EmptyHTTPResponseMatcher extends HTTPResponseMatcher{
		override def setResponseVerifier(matcher:Matcher):Unit = {}
		override def setRequestUrlVerifier(matcher:Matcher):Unit = {}
		override def setDurationVerifier(matcher:Matcher):Unit = {}
		override def setCodeVerifier(matcher:Matcher):Unit = {}
		override def setHeadersVerifier(matcher:Matcher):Unit = {}
		override def setRetriesVerifier(matcher:Matcher):Unit = {}
		override def setRedirectsVerifier(matcher:Matcher):Unit = {}
		override def setExceptionsVerifier(matcher:Matcher):Unit = {}
		override def verify(response:HTTPResponse):VerificationResponse = VerificationResponse(true,List.empty[String])
	}
	val empty = EmptyHTTPResponseMatcher
	def default = new HTTPResponseMatcher
}
case class VerificationResponse(success:Boolean,errors:List[String])
class HTTPResponseMatcher{
	protected var responseVerifier:Option[Matcher] = None
	def setResponseVerifier(matcher:Matcher):Unit = responseVerifier = Some(matcher)
	protected var requestUrlVerifier:Option[Matcher] = None
	def setRequestUrlVerifier(matcher:Matcher):Unit = requestUrlVerifier = Some(matcher)
	protected var durationVerifier:Option[Matcher] = None
	def setDurationVerifier(matcher:Matcher):Unit = durationVerifier = Some(matcher)
	protected var codeVerifier:Option[Matcher] = Some(IsNumericallyEqualsMatcher(200))
	def setCodeVerifier(matcher:Matcher):Unit = codeVerifier = Some(matcher)
	protected var headersVerifier:Option[Matcher] = None
	def setHeadersVerifier(matcher:Matcher):Unit = headersVerifier = Some(matcher)
	protected var retriesVerifier:Option[Matcher] = None
	def setRetriesVerifier(matcher:Matcher):Unit = retriesVerifier = Some(matcher)
	protected var redirectsVerifier:Option[Matcher] = None
	def setRedirectsVerifier(matcher:Matcher):Unit = redirectsVerifier = Some(matcher)
	protected var exceptionsVerifier:Option[Matcher] = None
	def setExceptionsVerifier(matcher:Matcher):Unit = exceptionsVerifier = Some(matcher)
	def verify(response:HTTPResponse):VerificationResponse = {
		var errors = List.empty[String]
		responseVerifier.map(m => {
			val responseString = response.responseAsString
			if (!m.verify(responseString))
				errors = errors ::: List("response (%s) failed verification %s".format(responseString,m.describe))
		})
		requestUrlVerifier.map(m => {
			if (!m.verify(response.requestUrl))
				errors = errors ::: List("requestUrl (%s) failed verification %s".format(response.requestUrl,m.describe))
		})
		durationVerifier.map(m => {
			if (!m.verify(response.duration))
				errors = errors ::: List("duration (%s) failed verification %s".format(response.duration,m.describe))
		})
		codeVerifier.map(m => {
			if (!m.verify(response.statusCode))
				errors = errors ::: List("statusCode (%s) failed verification %s".format(response.statusCode,m.describe))
		})
		headersVerifier.map(m => {
			val headersString = response.headers.toList.map(hi => "%s : %s".format(hi._1,hi._2)).mkString("\r\n")
			if (!m.verify(headersString))
				errors = errors ::: List("headers (%s) failed verification %s".format(headersString,m.describe))
		})
		retriesVerifier.map(m => {
			if (!m.verify(response.numberOfRetries))
				errors = errors ::: List("retries (%s) failed verification %s".format(response.numberOfRetries,m.describe))
		})
		redirectsVerifier.map(m => {
			if (!m.verify(response.numberOfRedirects))
				errors = errors ::: List("redirects (%s) failed verification %s".format(response.numberOfRedirects,m.describe))
		})
		exceptionsVerifier.map(m => {
			val exceptionsString = response.exceptions.map(ex => ex.getMessage).mkString("\r\n")
			if (!m.verify(exceptionsString))
				errors = errors ::: List("exceptions (%s) failed verification %s".format(exceptionsString,m.describe))
		})
		VerificationResponse(errors.length == 0, errors)	
	}
}

case class HttpCheck(serviceCheckMode:ServiceCheckMode, incomingLabel:String,uri:String,headers:List[Tuple2[String,String]] = List.empty[Tuple2[String,String]], matcher:HTTPResponseMatcher = HTTPResponseMatchers.default, time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
	def getClient = Http.getClient
  var client = getClient
	headers.foreach(h => client.addHttpHeader(h._1,h._2))
  override def resetEnvironment = {
    client = getClient
		headers.foreach(h => client.addHttpHeader(h._1,h._2))
  }
  def status = {
		val response = client.respondToResponse(client.getExpectingHTTPResponse(uri))
		val verificationResponse = matcher.verify(response)
		if (!verificationResponse.success){
			throw new DashboardException("HTTP Verification failed",verificationResponse.errors.mkString("\r\n"))
		}
		(response,Full(response.duration.toDouble))
	}
	override def performCheck = succeed(status._1.toString,status._2)
}
case class HttpCheckWithBasicAuth(serviceCheckMode:ServiceCheckMode, incomingLabel:String,uri:String,username:String,password:String, headers:List[Tuple2[String,String]] = List.empty[Tuple2[String,String]], matcher:HTTPResponseMatcher = HTTPResponseMatchers.default, time:TimeSpan = 5 seconds) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
	def getClient = Http.getAuthedClient(username,password)
  var client = getClient
	headers.foreach(h => client.addHttpHeader(h._1,h._2))
	override def resetEnvironment = {
    client = getClient
		headers.foreach(h => client.addHttpHeader(h._1,h._2))
  }
  def status = {
		val response = client.respondToResponse(client.getExpectingHTTPResponse(uri))
		val verificationResponse = matcher.verify(response)
		if (!verificationResponse.success){
			throw new DashboardException("HTTPwithAuth Verification failed",verificationResponse.errors.mkString("\r\n"))
		}
		(response,Full(response.duration.toDouble))
	}
	override def performCheck = succeed(status._1.toString,status._2)
}
case class DependencyCheck(serviceCheckMode:ServiceCheckMode,incomingLabel:String,dependencies:Map[DependencyDescription,DependencyMatcher],time:TimeSpan) extends Pinger(incomingLabel,serviceCheckMode){
	override val pollInterval = time
	failureTolerance = 3
	def status = {
		val errors = dependencies.toList.foldLeft(List.empty[String])((acc,item) => {
			val desc = item._1
			val matcher = item._2
			val result = matcher.verify(desc)
			acc ::: result.errors
		})
		if (errors.length > 0)
			throw new DashboardException("DependencyCheck Verification failed",errors.mkString("\r\n"))
		 (errors.length == 0).toString
	}
	override def performCheck = succeed(status)
}
case class MatcherCheck(serviceCheckMode:ServiceCheckMode,incomingLabel:String,matcher:Matcher,time:TimeSpan) extends Pinger(incomingLabel,serviceCheckMode){
	override val pollInterval = time
	failureTolerance = 3
	def status = "%s is %s".format(matcher.describe,matcher.verify(true).toString)
	override def performCheck = succeed(status)
}

case class ScriptStepResult(body:String,metaData:Map[String,String] = Map.empty[String,String],statusCode:Int = 0,duration:Double = 0.0)

case class FunctionalCheckReturn(result:ScriptStepResult,duration:Double,updatedEnvironment:Map[String,String],data:List[Tuple2[Long,Map[String,GraphableDatum]]],sqlResult:Option[SQLResultSet] = None,httpResult:Option[HTTPResponse] = None,ldapResults:Option[LdapResults] = None,jmxResults:Option[JmxResults] = None){
  protected def safeDisplay(in:String):String = {
    in match {
      case null => ""
      case s:String if s.length > 100 => s.take(100)
      case s:String => s
    }
  }
  override def toString = {
    "StepResult(%s,%s,%s)".format(safeDisplay(result.body),duration,updatedEnvironment.map(t => (t._1,safeDisplay(t._2))))
  }
}

object ScriptStepResult {
  def empty = ScriptStepResult("")
}
object FunctionalCheckReturn {
  def empty = FunctionalCheckReturn(ScriptStepResult.empty,0.0,Map.empty[String,String],Nil)
}

object FunctionalCheck extends ConfigFileReader {
  def configureFromXml(n:Node):List[FunctionalCheck] = {
		getImmediateNodes(n,"step").flatMap(mn => {
			getAttr(mn,"type").getOrElse("unknown") match {
				case "http" => {
          for (
            url <- getAttr(mn,"url");
            method <- getAttr(mn,"method");
            params = getNodes(mn,"parameter").flatMap(pn => {
              for (
                key <- getAttr(pn,"key");
                value <- getAttr(pn,"value")
              ) yield {
                (key,value)
              }
            });
            headers = Map(getNodes(mn,"header").flatMap(hn => {
              for (
                key <- getAttr(hn,"key");
                value <- getAttr(hn,"value")
              ) yield {
                (key,value)
              } 
            }):_*);
						matcher:HTTPResponseMatcher = HTTPResponseMatchers.configureFromXml(<thresholdsPacket>{getNodes(mn,"thresholds")}</thresholdsPacket>)
          ) yield HttpFunctionalCheck(method,url,params,headers,matcher)
        }
        case "sql" => {
          for (
            driver <- getAttr(mn,"driver");
            url <- getAttr(mn,"url");
            username <- getAttr(mn,"username");
            password <- getAttr(mn,"password");
            query <- getAttr(mn,"query");
            connectionTimeout = getAttr(mn,"connectionTimeout").map(_.toLong);
            matchers: List[VerifiableSqlResultSetDefinition] = getNodes(mn,"thresholds").map(ts => {
							val rowBehaviour = getAttr(ts,"rows").getOrElse("all")
							val tsMap = Matchers.configureFromXml(ts)
							VerifiableSqlResultSetDefinition(rowBehaviour,tsMap)
						})
          ) yield JDBCFunctionalCheck(driver,url,username,password,query,matchers,connectionTimeout.getOrElse(10000L))
        }
        case "ldap" => {
          for (
            host <- getAttr(mn,"url");
            username <- getAttr(mn,"username");
            password <- getAttr(mn,"password");
            searchBase <- getAttr(mn,"searchBase");
            query <- getAttr(mn,"query")
          ) yield LdapFunctionalCheck(host,username,password,searchBase,query)
        }
        case "icmp" => {
          for (
            host <- getAttr(mn,"host");
            ipv6 = getAttr(mn,"ipv6").map(_.toBoolean).getOrElse(false)
          ) yield ICMPFunctionalCheck(host,ipv6)
        }
        case "jmx" => {
          for (
            url <- getAttr(mn,"url")
          ) yield JmxFunctionalCheck(url)
        }
        case "munin" => {
          for (
            host <- getAttr(mn,"host");
            port <- getAttr(mn,"port").map(_.toInt)
          ) yield {
            (mn \\ "thresholds").headOption.map(_ho => getNodes(mn,"thresholds").map(ts => {
              val tsName = getAttr(ts,"name").getOrElse("unknown")
              val tsType = getAttr(ts,"type").getOrElse("counter")
              MuninCategoryDefinition(tsName,MuninFieldType.parse(tsType))
            })).map(onlyFetch => {
              MuninFunctionalCheck(host,port,onlyFetch)
            }).getOrElse({
              MuninFunctionalCheck(host,port)
            })
          }
        }
        case "latestDataExtractor" => {
          for (
            key <- getAttr(mn,"key");
            dataAttributeName <- getAttr(mn,"dataAttributeName")
          ) yield {
            LatestDataExtractor(key,dataAttributeName)
          }
        }
        case "lastDataExtractor" => {
          for (
            key <- getAttr(mn,"key");
            dataAttributeName <- getAttr(mn,"dataAttributeName")
          ) yield {
            LastDataExtractor(key,dataAttributeName)
          }
        }
        case "jmxExtractOsLoad" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxOsLoadExtractor(key)
          }
        }
        case "jmxExtractOsProcessorCount" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxOsProcessorCountExtractor(key)
          }
        }
        case "jmxExtractOsName" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxOsNameExtractor(key)
          }
        }
        case "jmxExtractOsArch" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxOsArchExtractor(key)
          }
        }
        case "jmxExtractOsVersion" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxOsVersionExtractor(key)
          }
        }
        case "jmxExtractRuntimeUptime" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxRuntimeUptimeExtractor(key)
          }
        }
        case "jmxExtractRuntimeInputArgs" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxRuntimeInputArgsExtractor(key)
          }
        }
        case "jmxExtractRuntimeName" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxRuntimeNameExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryMax" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxHeapMemoryMaxExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryUsed" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxHeapMemoryUsedExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryCommitted" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxHeapMemoryCommittedExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryInit" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxHeapMemoryInitExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryPercentage" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxHeapMemoryPercentageExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryMax" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxNonHeapMemoryMaxExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryUsed" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxNonHeapMemoryUsedExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryCommitted" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxNonHeapMemoryCommittedExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryInit" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxNonHeapMemoryInitExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryPercentage" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            JmxNonHeapMemoryPercentageExtractor(key)
          }
        }

        case "httpAttachBasicAuth" => {
          for (
            domain <- getAttr(mn,"domain");
            username <- getAttr(mn,"username");
            password <- getAttr(mn,"password")
          ) yield HttpAddBasicAuthorization(domain,username,password)
        }
        case "httpExtractRequestUrl" => {
          for (
            key <- getAttr(mn,"key")
          ) yield HttpRequestUrlExtractor(key)
        }
        case "httpExtractStatusCode" => {
          for (
            key <- getAttr(mn,"key")
          ) yield HttpStatusCodeExtractor(key)
        }
        case "httpExtractHeaderValue" => {
          for (
            key <- getAttr(mn,"key");
            header <- getAttr(mn,"header")
          ) yield HttpHeaderExtractor(key,header)
        }
        case "httpExtractRedirectCount" => {
          for (
            key <- getAttr(mn,"key")
          ) yield HttpRedirectCountExtractor(key)
        }
        case "httpExtractRetryCount" => {
          for (
            key <- getAttr(mn,"key")
          ) yield HttpRetryCountExtractor(key)
        }
        case "httpExtractStartTime" => {
          for (
            key <- getAttr(mn,"key")
          ) yield HttpStartTimeExtractor(key)
        }
        case "httpExtractEndTime" => {
          for (
            key <- getAttr(mn,"key")
          ) yield HttpEndTimeExtractor(key)
        }
        case "httpExtractExceptions" => {
          for (
            key <- getAttr(mn,"key")
          ) yield HttpExceptionsExtractor(key)
        }
        case "storeSqlResultSet" => {
          for (
            key <- getAttr(mn,"key")
          ) yield StoreSqlResultSet(key)
        }
        case "sqlExtractRow" => {
          for (
            key <- getAttr(mn,"key");
            row <- getAttr(mn,"row").map(_.toInt);
            separator = getAttr(mn,"separator")
          ) yield SqlRowExtractor(key,row)
        }
        case "sqlExtractColumn" => {
          for (
            key <- getAttr(mn,"key");
            col <- getAttr(mn,"column");
            separator = getAttr(mn,"separator")
          ) yield SqlColExtractor(key,col,separator)
        }
        case "sqlExtractCell" => {
          for (
            key <- getAttr(mn,"key");
            row <- getAttr(mn,"row").map(_.toInt);
            col <- getAttr(mn,"column")
          ) yield {
            SqlCellExtractor(key,row,col)
          }
        }
        case "storeLdapResult" => {
          for (
            key <- getAttr(mn,"key")
          ) yield StoreLdapResults(key)
        }
        case "ldapExtractQuery" => {
          for (
            key <- getAttr(mn,"key")
          ) yield LdapQueryExtractor(key)
        }
        case "ldapExtractSearchBase" => {
          for (
            key <- getAttr(mn,"key")
          ) yield LdapSearchBaseExtractor(key)
        }
        case "ldapExtractRecord" => {
          for (
            key <- getAttr(mn,"key");
            recordName <- getAttr(mn,"recordName")
          ) yield LdapRecordExtractor(key,recordName)
        }
        case "ldapExtractAttr" => {
          for (
            key <- getAttr(mn,"key");
            attrName <- getAttr(mn,"attrName")
          ) yield LdapAttrExtractor(key,attrName)
        }
        case "ldapExtractAttrFromRecord" => {
          for (
            key <- getAttr(mn,"key");
            recordName <- getAttr(mn,"recordName");
            attrName <- getAttr(mn,"attrName")
          ) yield LdapAttrFromRecordExtractor(key,recordName,attrName)
        }
      
        case "setKey" => {
          for (
            key <- getAttr(mn,"key");
            value <- getAttr(mn,"value")
          ) yield {
            KeySetter(key,value)
          }
        }
        case "deleteKey" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            KeyDeleter(key)
          }
        }
        case "storeResult" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            ResultStorer(key)
          }
        }
        case "storeMetaData" => {
          for (
            key <- getAttr(mn,"key");
            metaDataKey <- getAttr(mn,"metaDataKey")
          ) yield {
            MetaDataStorer(key,metaDataKey)
          }
        }
        case "storeStatusCode" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            StatusCodeStorer(key)
          }
        }
        case "xPathResult" => {
          for (
            key <- getAttr(mn,"key");
            xPath <- getAttr(mn,"xPath")
          ) yield {
            XPathFromResult(key,xPath)
          }
        }
        case "foreachXPathResult" => {
          for (
            key <- getAttr(mn,"key");
            xPath <- getAttr(mn,"xPath");
            funcs = getImmediateNodes(mn,"do").flatMap(tn => configureFromXml(tn))
          ) yield {
            ForeachXPathFromResult(key,xPath,funcs)
          }
        }
        case "regexResult" => {
          for (
            key <- getAttr(mn,"key");
            regex <- getAttr(mn,"regex")
          ) yield {
            RegexFromResult(key,regex)
          }
        }
        case "foreachRegexResult" => {
          for (
            key <- getAttr(mn,"key");
            regex <- getAttr(mn,"regex");
            funcs = getImmediateNodes(mn,"do").flatMap(tn => configureFromXml(tn))
          ) yield {
            ForeachRegexFromResult(key,regex,funcs)
          }
        }
        case "setResult" => {
          for (
            result <- getAttr(mn,"result")
          ) yield {
            ResultSetter(result)
          }
        }
        case "setResultToAttribute" => {
          for (
            key <- getAttr(mn,"key")
          ) yield {
            RetrieveAttributeToResult(key)
          }
        }
        case "if" => {
          for (
            key <- getAttr(mn,"key");
            value <- getAttr(mn,"value");
            thenFuncs = getImmediateNodes(mn,"then").flatMap(tn => configureFromXml(tn));
            elseFuncs = getImmediateNodes(mn,"else").flatMap(tn => configureFromXml(tn))
          ) yield {
            Cond(key,value,thenFuncs,elseFuncs)  
          }
        }
        case "while" => {
          for (
            key <- getAttr(mn,"key");
            value <- getAttr(mn,"value");
            funcs = getImmediateNodes(mn,"do").flatMap(tn => configureFromXml(tn))
          ) yield {
            WhileLoop(key,value,funcs)  
          }
        }
        case "for" => {
          for (
            key <- getAttr(mn,"key");
            start <- getAttr(mn,"start").map(_.toInt);
            end <- getAttr(mn,"end").map(_.toInt);
            incrementing = getAttr(mn,"incrementing").map(_.trim.toLowerCase == "true").getOrElse(true);
            funcs = getImmediateNodes(mn,"do").flatMap(tn => configureFromXml(tn))
          ) yield {
            ForLoop(key,start,end,incrementing,funcs)
          }
        }
        case "delay" => {
          for (
            delay <- getAttr(mn,"delay").map(_.toLong);
            random = getAttr(mn,"random").map(_.trim.toLowerCase == "true").getOrElse(false)
          ) yield {
            Delay(delay,random)
          }
        }
        case "stringEquals" => {
          for (
            key <- getAttr(mn,"key");
            value <- getAttr(mn,"value")
          ) yield {
            EnvironmentValidator("%s equals %s".format(key,value),(env) => env.get(key).exists(_ == value))
          }
        }
        case "stringEqualsInsensitive" => {
          for (
            key <- getAttr(mn,"key");
            value <- getAttr(mn,"value").map(_.toLowerCase.trim)
          ) yield {
            EnvironmentValidator("%s equals %s".format(key,value),(env) => env.get(key).exists(_.toLowerCase.trim == value))
          }
        }
        case "numericallyLessThan" => {
          for (
            key <- getAttr(mn,"key");
            value <- getAttr(mn,"value").map(_.toDouble)
          ) yield {
            EnvironmentValidator("%s numerically less than %s".format(key,value),(env) => env.get(key).exists(_.toDouble < value))
          }
        }
        case "numericallyGreaterThan" => {
          for (
            key <- getAttr(mn,"key");
            value <- getAttr(mn,"value").map(_.toDouble)
          ) yield {
            EnvironmentValidator("%s numerically less than %s".format(key,value),(env) => env.get(key).exists(_.toDouble > value))
          }
        }
        case "numericallyEqualTo" => {
          for (
            key <- getAttr(mn,"key");
            value <- getAttr(mn,"value").map(_.toDouble)
          ) yield {
            EnvironmentValidator("%s numerically less than %s".format(key,value),(env) => env.get(key).exists(_.toDouble == value))
          }
        }

				case _ => None
			}
		})
	}
}

import GraphableData._

abstract class FunctionalCheck extends Logger {
  protected var see:Option[ScriptExecutionEnvironment] = None
  def attachScriptExecutionEnvironment(newSee:ScriptExecutionEnvironment) = {
    see = Some(newSee)
  }
  protected def innerAct(previousResult:FunctionalCheckReturn,interpolator:Interpolator):FunctionalCheckReturn 
  def act(previousResult:FunctionalCheckReturn,interpolator:Interpolator):Either[Exception,FunctionalCheckReturn] = try {
    debug("STEP: %s \r\n (env: %s)".format(this,previousResult.updatedEnvironment))
    Right(innerAct(previousResult,interpolator))
  } catch {
    case e:Exception => Left(e)
  }
}

case class HttpAddBasicAuthorization(domain:String,username:String,password:String) extends FunctionalCheck {
  override protected def innerAct(previousResult:FunctionalCheckReturn,interpolator:Interpolator) = {
    see.foreach(s => {
      s.httpClient.addAuthorization(interpolator.interpolate(domain,previousResult.updatedEnvironment),interpolator.interpolate(username,previousResult.updatedEnvironment),interpolator.interpolate(password,previousResult.updatedEnvironment))   
    })
    previousResult
  }
}
case class ICMPFunctionalCheck(uri:String,ipv6:Boolean = false) extends FunctionalCheck {
		//pinging is done via ipv4 at present.  ipv6 in some networks results in unexpected results for some subnets
   protected def pingCmd(url:String) = {
     ((ServiceConfigurator.isWindows,ServiceConfigurator.isLinux,ServiceConfigurator.isOSX,ipv6) match {
       case (true,false,false,false) => "ping -4 -n 1 "
       case (true,false,false,true) => "ping -6 -n 1 "
       case (false,true,false,false) => "ping -c 1 "
       case (false,true,false,true) => "ping6 -c 1 "
       case (false,false,true,false) => "ping -c 1 "
       case (false,false,true,true) => "ping6 -c 1 "
       case _ => "ping -c 1 "
     }) + url	
   }
	 private val pingTimeExtractor:String=>Box[Double] = (ServiceConfigurator.isWindows,ServiceConfigurator.isLinux,ServiceConfigurator.isOSX) match {
		case (true,false,false) => (output:String) => {
			val reg = """time([=<])([0-9]+)ms""".r
			reg.findFirstMatchIn(output) match {
				case Some(timeMatch) => {
					val timeString = timeMatch.group(2)
					timeMatch.group(1) match {
						case "=" => tryo(timeString.toLong)
					//approximating time to 0.95 of time, if the ping response read "time<1ms" instead of "time=1ms"
						case "<" => tryo(timeString.toLong * 0.95)
					}
				}
				case _ => Empty
			}
		}
		case (false,true,false) => (output:String) => {
			val reg = """time[=<]([0-9.]+) ms""".r
			reg.findFirstMatchIn(output) match {
				case Some(timeMatch) => {
					val timeString = timeMatch.group(1)
					tryo(timeString.toDouble)
				}
				case _ => Empty
			}
		}
		case (false,false,true) => (output:String) => {
			val reg = """time[=<]([0-9.]+) ms""".r
			reg.findFirstMatchIn(output) match {
				case Some(timeMatch) => {
					val timeString = timeMatch.group(1)
					tryo(timeString.toDouble)
				}
				case _ => Empty
			}
		}
		case _ => (output:String) => Empty
	}	
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val now = new Date().getTime
    val pingProcess = Runtime.getRuntime().exec(pingCmd(interpolator.interpolate(uri,environment)))
    val inputStream = new BufferedInputStream(pingProcess.getInputStream)
    val errorStream = new BufferedInputStream(pingProcess.getErrorStream)
    var output = ""
    pingProcess.waitFor
    while (errorStream.available > 0 || inputStream.available > 0 )
    {
      while (inputStream.available > 0)
        output += inputStream.read.asInstanceOf[Char]
      while (errorStream.available > 0)
        output += errorStream.read.asInstanceOf[Char]
    }
    pingProcess.destroy
    if (output.length == 0)
      throw new DashboardException("Ping failed","ping command failed - no response from OS")
    if (output.contains("cannot resolve") || output.contains("Unknown host") || output.contains("could not find host"))
      throw new DashboardException("Ping failed","Unknown host: "+output)
    if (!(output.contains(" 0% packet loss") || output.contains("(0% loss)")))
      throw new DashboardException("Ping failed","Packet loss recognised: "+output)
    val stringOutput = output.toString
    val timeTaken = pingTimeExtractor(stringOutput)
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now,Map(
      "checkType" -> "icmp",
      "ipv6" -> ipv6,
      "timeTaken" -> timeTaken.getOrElse(0.0)
    ))
    FunctionalCheckReturn(ScriptStepResult(body = stringOutput,duration = timeTaken.openOr(0)),totalDuration + timeTaken.openOr(0.0),environment,newData :: fcr.data)
  }
}

object JDBCFunctionalCheckDriverInitializations extends Logger {
  protected var initializedDrivers = List.empty[String]
  def initialize(driver:String) = {
    try {
      this.synchronized {
        if (!initializedDrivers.contains(driver)){
          Class.forName(driver).newInstance()
          initializedDrivers = driver :: initializedDrivers
        }
      }
    } catch {
      case e:Exception => {
        error("exception initializing JDBC driver",e)
      }
    }
  }
}

case class JDBCFunctionalCheck(driver:String,url:String,username:String,password:String,query:String,thresholds:List[VerifiableSqlResultSetDefinition] = List.empty[VerifiableSqlResultSetDefinition],connectionCreationTimeout:Long = 10000L) extends FunctionalCheck {
  JDBCFunctionalCheckDriverInitializations.initialize(driver)
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
		var output = SQLResultSet(Map.empty[Int,SQLRow])
    var start = new java.util.Date().getTime
    var timeTaken:Box[Double] = Empty
		var errors = List.empty[Throwable]
		try {
      Await.result(Future(Some({
				val result = try {
					val conn = DriverManager.getConnection(url,username,password)//"jdbc:oracle:thin:@%s".format(uri),username,password)
					val statement = conn.createStatement
					var failedVerificationResponses = List.empty[VerificationResponse]
					val resultSet = statement.executeQuery(query)
					output = VerifiableSQLResultSetConverter.toVerifiableSQLResultSet(resultSet)
					resultSet.close
					conn.close
          timeTaken = Full(new java.util.Date().getTime - start)
					val verificationResponses = thresholds.map(t => t.verifyResultSet(output))
					failedVerificationResponses = verificationResponses.filter(pfv => !pfv.success)
					if (failedVerificationResponses.length > 0){
						errors = errors ::: List(new DashboardException("SQL Verification failed",failedVerificationResponses.map(fvr => fvr.errors).flatten.mkString("\r\n")))
						(false,conn) 
					} else {
						(true,conn)
					}
				} catch {
					case e:Throwable => {
            timeTaken = Full(new java.util.Date().getTime - start)
						errors = errors ::: List(e)
						(false,null.asInstanceOf[java.sql.Connection])	
					}
				}
        result match {
          case l:List[Option[Option[Tuple2[Boolean,Connection]]]] if l.length > 0 => l.head match {
            case Some(Some((_,null))) => {
              errors = errors ::: List(new DashboardException("SQL Connection failed","connection is null"))
            }
            case Some(Some((true,connection))) => {
            }
            case Some(Some((false,other))) => {
              errors = errors ::: List(new DashboardException("SQL Connection failed","connection: %s".format(other.toString)))
            }
            case other => {
              errors = errors ::: List(new DashboardException("SQL Connection failed","other: %s".format(other.toString)))
            }
          }
        }
			})),Duration(connectionCreationTimeout,"millis")) 
    } catch {
      case e:TimeoutException => {
        timeTaken = Full(new java.util.Date().getTime - start)
        errors = errors ::: List(new DashboardException("SQL Connection failed","oracle failed to create a connection within the timeout",List(e)))
      }
    }
		if (errors.length == 1) {
			throw errors.head
		} else if (errors.length > 0) {
			throw new CombinedExceptionsException(errors)
		} else { 
      val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now.getTime,Map(("checkType",GraphableString("jdbc")) :: timeTaken.toList.map(tt => {
        ("timeTaken",GraphableDouble(tt))
      }):_*))
      FunctionalCheckReturn(
        result = ScriptStepResult(body = output.toString,duration = timeTaken.openOr(0)),
        duration = totalDuration + timeTaken.openOr(0.0),
        updatedEnvironment = environment,
        sqlResult = Some(output),
        data = newData :: fcr.data
      )
    }
  }
}

case class LdapResults(query:String,base:String,results:List[LdapResult])
case class LdapResult(name:String,attrs:List[LdapAttr])
case class LdapAttr(name:String,values:List[String])


case class LdapFunctionalCheck(host:String,username:String,password:String,searchBase:String,query:String) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val start = new java.util.Date().getTime
    var error:Option[Exception] = None
    val env = new java.util.Hashtable[String,String]()
    env.put(Context.INITIAL_CONTEXT_FACTORY,"com.sun.jndi.ldap.LdapCtxFactory")
                    env.put(Context.PROVIDER_URL,interpolator.interpolate(host,environment))
                    env.put(Context.SECURITY_AUTHENTICATION,"simple")
                    env.put(Context.SECURITY_PRINCIPAL,interpolator.interpolate(username,environment))
                   
                    env.put(Context.SECURITY_CREDENTIALS,interpolator.interpolate(password,environment))
    var ctx = new InitialDirContext(env)

    val intQuery = interpolator.interpolate(query,environment)
    val intSearchBase = interpolator.interpolate(searchBase,environment)

    var output = LdapResults(intQuery,intSearchBase,Nil)
    try {

      val controls = new SearchControls
      controls.setSearchScope(SearchControls.SUBTREE_SCOPE)

      ctx = new InitialDirContext(env)
      val results = ctx.search(intSearchBase,intQuery,controls)
      while (results.hasMore){
        val row = results.next()
        val rowAttrs = row.getAttributes().getAll()
        var attrs = List.empty[LdapAttr]
        while (rowAttrs.hasMore){
          rowAttrs.next() match {
            case attr:javax.naming.directory.Attribute => {
              val attrValues = attr.getAll()
              var aValues = List.empty[String]
              while (attrValues.hasMore){
                val value = attrValues.next()
                aValues = aValues ::: List(value.toString)
              }
              attrs = attrs ::: List(LdapAttr(attr.getID(),aValues))
            }
            case _ => {}
          }
        }
        output = output.copy(results = output.results ::: List(LdapResult(row.getNameInNamespace,attrs)))
      }
      results.close
    } catch {
      case e:Exception => error = Some(e)
    } finally {
      ctx.close
    }
    error.foreach(e => throw e)
    val duration = new java.util.Date().getTime - start
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now.getTime,Map(
      "checkType" -> "ldap",
      "timeTaken" -> duration
    ))
    FunctionalCheckReturn(
      result = ScriptStepResult(output.toString,Map.empty[String,String],0,duration.toDouble),
      duration = totalDuration + duration,
      updatedEnvironment = environment,
      ldapResults = Some(output),
      data = newData :: fcr.data
    )
  }
}


case class HttpFunctionalCheck(method:String,url:String,parameters:List[Tuple2[String,String]] = Nil,headers:Map[String,String] = Map.empty[String,String],matcher:HTTPResponseMatcher = HTTPResponseMatchers.empty) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val client = see.map(_.httpClient).getOrElse({
      throw new Exception("no available httpClient")
    })
    val interpolatedHeaders = headers.map(h => (h._1,interpolator.interpolate(h._2,environment)))
    interpolatedHeaders.foreach(h => client.addHttpHeader(h._1,interpolator.interpolate(h._2,environment)))
    val interpolatedUrl = interpolator.interpolate(url,environment)
    val interpolatedParameters = parameters.map(p => (interpolator.interpolate(p._1,environment),interpolator.interpolate(p._2,environment)))
    val innerResponse = method.trim.toLowerCase match {
      case "get" => client.getExpectingHTTPResponse(interpolatedUrl)
      case "post" => client.postFormExpectingHTTPResponse(interpolatedUrl,interpolatedParameters)
      case unsupportedMethod => throw new DashboardException("HTTP method not supported","%s [%s => %s] ([%s => %s],%s)".format(unsupportedMethod,url,interpolatedUrl,parameters,interpolatedParameters,headers))
    }
    val response = client.respondToResponse(innerResponse)
    val verificationResponse = matcher.verify(response)
    if (!verificationResponse.success){
      throw new DashboardException("HTTP Verification failed",verificationResponse.errors.mkString("\r\n"))
    }
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now.getTime,Map(
      "checkType" -> "http",
      "timeTaken" -> response.duration.toDouble,
      "statusCode" -> response.statusCode
    ))
    FunctionalCheckReturn(
      result = ScriptStepResult(response.responseAsString,response.headers,response.statusCode,response.duration.toDouble),
      duration = totalDuration + response.duration,
      updatedEnvironment = environment,
      httpResult = Some(response),
      data = newData :: fcr.data
    )
  }
}

class TelnetFunctionalCheck[A](host:String,port:Int) extends FunctionalCheck {
  protected val commandResponseTerminator:Option[String] = None
  protected def telnetBehaviour(tc:TelnetClient):Tuple2[List[String],Option[A]] = {
    val inputStream = new BufferedInputStream(tc.getInputStream)
    val output = readStream(inputStream)	
    inputStream.close
    (output.split("\n").toList,None)
  }
  protected def writeTo(input:String,stream:BufferedOutputStream) = {
    val command = (input + "\n").getBytes("ISO-8859-1")
    stream.write(command,0,command.length)
    stream.flush	
  }
  protected def readStream(input:BufferedInputStream,endPattern:Option[String] = None):String = readStreamLoop(input,endPattern,new Date(),5000L)
  protected def readStreamLoop(input:BufferedInputStream,endPattern:Option[String],startTime:Date, timeout:Long):String = {
    var output = ""
    var hasFinished = false
    while (input.available > 0 && !hasFinished){
      output += input.read.asInstanceOf[Char]
      endPattern.map(ep => {
        hasFinished = output.endsWith(ep)
      })
    }
    if (output == "" && (new Date().getTime - startTime.getTime) < timeout){
      Thread.sleep(timeout / 10)	
      output += readStreamLoop(input,endPattern,startTime,timeout)
    }
    output
  }
  protected def convert(in:A):Map[String,GraphableDatum] = Map.empty[String,GraphableDatum]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val now = new Date().getTime
    val tc = new TelnetClient()
    tc.connect(interpolator.interpolate(host,fcr.updatedEnvironment),port)
    val output = telnetBehaviour(tc)
    tc.disconnect
    val duration = (new Date().getTime - now).toDouble
    val carrierData = output._2.map(convert _).getOrElse(Map.empty[String,GraphableDatum])
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now,Map(
      "checkType" -> GraphableString("telnet"),
      "timeTaken" -> GraphableDouble(duration),
      "statusCode" -> GraphableInt(200)
    ) ++ carrierData)
    FunctionalCheckReturn(
      result = ScriptStepResult(output._1.mkString("\r\n"),Map.empty[String,String],200,duration),
      duration = fcr.duration + duration,
      updatedEnvironment = fcr.updatedEnvironment,
      data = newData :: fcr.data
    )
  }
}

case class MuninFunctionalCheck(host:String, port:Int, onlyFetch:List[MuninCategoryDefinition] = List(MuninCategoryDefinition("cpu",Counter),MuninCategoryDefinition("memory",Guage))) extends TelnetFunctionalCheck[Map[String,Map[String,Double]]](host,port){
	protected val previous = {
		val map = new MSMap[String,MSMap[String,Double]]()
		onlyFetch.map(munCatDef => map.put(munCatDef.name,new MSMap[String,scala.Double]()))
		map
	}
	override val commandResponseTerminator:Option[String] = Some("\n.\n")
	protected def generatedDelta[Double](inputName:String,input:Map[String,scala.Double]):Map[String,scala.Double] = {
		val result = previous.get(inputName).map(po => Map(input.keys.map(ink => {
			val updatedValue = (po(ink),input(ink)) match {
				case (p:scala.Double,i:scala.Double) if (i < p) => {
					// this is the counter reset behaviour.  I do believe that counters occasionally reset themselves to zero in munin
					debug("possibleOverflow: %s (%s -> %s)".format(ink,p,i))
					val out = i
					po.put(ink,0.0)
					out
				}
				case (p:scala.Double,i:scala.Double) => {
					debug("correct counter behaviour: %s (%s -> %s)".format(ink,p,i))
					val out = i - p
					po.put(ink,i)
					out
				}
				case other => {
					debug("resetting to zero: %s (%s)".format(ink,other))
					val out = 0.0
					po.put(ink,0.0)
					out
				}
			}
			(ink,updatedValue)
		}).toList:_*)).getOrElse(input)
		result
	}
	protected def interpretMuninData(tc:TelnetClient):Map[String,Map[String,Double]] = {
		val outputStream = new BufferedOutputStream(tc.getOutputStream)
		val inputStream = new BufferedInputStream(tc.getInputStream)
		var output = readStream(inputStream)
		if (output.length == 0)
			throw new DashboardException("Munin failed","no response from remote node")
		writeTo("list",outputStream)
		val possibleQueries = readStream(inputStream).split(" ").toList
		val desiredQueries = onlyFetch.filter(of => possibleQueries.contains(of.name))
		val completeOutput = Map(desiredQueries.map(of => {
			val o = of.name
			writeTo("fetch "+o,outputStream)
			val individualOutput = readStream(inputStream,commandResponseTerminator)
			val formattedOutput = Map(individualOutput.split("\n").filter(l => l.length > 1).map(l => {
				val parts = l.split(" ")
				val muninDataKey = parts(0).reverse.dropWhile(c => c != '.').drop(1).reverse.toString
				val muninDataValue = tryo(parts(1).toDouble).openOr(-1.0)
				(muninDataKey,muninDataValue)
			}):_*)
			val finalOutput = of.fieldType match {
				case Counter => {
					try {
						val deltas = generatedDelta(o, formattedOutput)
						Map(deltas.toList.map(fo => (fo._1,fo._2.toDouble)):_*)
					} catch {	
						case _ => formattedOutput
					}
				}
				case Guage => {
					formattedOutput
				}
				case PercentageCounter => {
					try {
						val deltas = generatedDelta(o,formattedOutput)
						val total = deltas.values.sum
						val deltaPercentages = Map(deltas.toList.map(d => (d._1, ((d._2 / total) * 100))):_*)
						deltaPercentages
					} catch {
						case _ => formattedOutput
					}
				}
				case PercentageGuage => {
					try {
						val total = formattedOutput.values.sum
						Map(formattedOutput.toList.map(fo => (fo._1,((fo._2 / total) * 100))):_*)
					} catch {
						case _ => formattedOutput
					}
				}
			}
			(o,finalOutput)	
		}):_*)
		writeTo("quit",outputStream)
		outputStream.close
		inputStream.close
		completeOutput
	}
  override protected def convert(in:Map[String,Map[String,Double]]):Map[String,GraphableDatum] = {
    Map(in.toList.flatMap(ot => {
      ot._2.toList.map(it => {
        ("%s__%s".format(ot._1,it._1),GraphableDouble(it._2))
      })
    }):_*)
  }
	override def telnetBehaviour(tc:TelnetClient):Tuple2[List[String],Option[Map[String,Map[String,Double]]]] = {
		val completeOutput = interpretMuninData(tc)
		(completeOutput.keys.map(cok => "%s -> %s".format(cok,completeOutput(cok))).toList,Some(completeOutput))
	}
}

case class JmxFunctionalCheck(jmxServiceUrl:String,credentials:Option[Tuple2[String,String]] = None) extends FunctionalCheck {
  import java.lang.Thread.State
  import java.lang.management._
  import java.lang.management.ManagementFactory._
  import javax.management.remote.{JMXServiceURL,JMXConnectorFactory}
  import collection.JavaConverters._
  override def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val interpolatedUrl = new JMXServiceURL(interpolator.interpolate(jmxServiceUrl,environment))
    
    val start = new java.util.Date().getTime
    val client = JMXConnectorFactory.connect(interpolatedUrl)
    val beanConnection = client.getMBeanServerConnection()

    val remoteOsBean = ManagementFactory.newPlatformMXBeanProxy(beanConnection,
      ManagementFactory.OPERATING_SYSTEM_MXBEAN_NAME,
      classOf[OperatingSystemMXBean]
    )
    val remoteOs = JmxOsSpec(remoteOsBean.getName,remoteOsBean.getArch,remoteOsBean.getAvailableProcessors,remoteOsBean.getSystemLoadAverage,remoteOsBean.getVersion)
    val remoteRuntimeBean = ManagementFactory.newPlatformMXBeanProxy(beanConnection,
      ManagementFactory.RUNTIME_MXBEAN_NAME,
      classOf[RuntimeMXBean]
    )
    val remoteRuntime = JmxRuntimeSpec(
      remoteRuntimeBean.getName,
      remoteRuntimeBean.getInputArguments.asScala.toList,//.toArray.toList.map(_.asInstanceOf[String]),
      remoteRuntimeBean.getClassPath,
      remoteRuntimeBean.getLibraryPath,
      remoteRuntimeBean.getManagementSpecVersion,
      remoteRuntimeBean.getSpecName,
      remoteRuntimeBean.getSpecVendor,
      remoteRuntimeBean.getSpecVersion,
      remoteRuntimeBean.getSystemProperties.asScala.toMap,
      remoteRuntimeBean.getUptime,
      remoteRuntimeBean.getVmName,
      remoteRuntimeBean.getVmVendor,
      remoteRuntimeBean.getVmVersion,
      remoteRuntimeBean.getStartTime,
      remoteRuntimeBean.isBootClassPathSupported match {
        case true => Some(remoteRuntimeBean.getBootClassPath)
        case false => None
      }
    )
    val remoteMemoryBean = ManagementFactory.newPlatformMXBeanProxy(beanConnection,
      ManagementFactory.MEMORY_MXBEAN_NAME,
      classOf[MemoryMXBean]
    )
    val remoteHeapBean = remoteMemoryBean.getHeapMemoryUsage
    val remoteNonHeapBean = remoteMemoryBean.getNonHeapMemoryUsage
    val remoteMemory = JmxMemorySpec(
      JmxMemoryUsage(
        remoteHeapBean.getInit,
        remoteHeapBean.getUsed,
        remoteHeapBean.getCommitted,
        remoteHeapBean.getMax
      ),
      JmxMemoryUsage(
        remoteNonHeapBean.getInit,
        remoteNonHeapBean.getUsed,
        remoteNonHeapBean.getCommitted,
        remoteNonHeapBean.getMax
      ),
      remoteMemoryBean.getObjectPendingFinalizationCount
    )
    val remoteThreadBean = ManagementFactory.newPlatformMXBeanProxy(beanConnection,
      ManagementFactory.THREAD_MXBEAN_NAME,
      classOf[ThreadMXBean]
    )
    val cpuTimeSupported = remoteThreadBean.isThreadCpuTimeSupported && {
      true 
    }
    val allThreadIds = remoteThreadBean.getAllThreadIds
    val cpuTimes = remoteThreadBean.isThreadCpuTimeSupported match {
      case false => Map.empty[Long,Long]
      case true => {
        var enabledThreadCpuTime = false
        if (!remoteThreadBean.isThreadCpuTimeEnabled){
          remoteThreadBean.setThreadCpuTimeEnabled(true)
          enabledThreadCpuTime = true
        }
        val threadTimes = Map(allThreadIds.toList.map(tid => (tid,remoteThreadBean.getThreadCpuTime(tid))):_*)
        if (enabledThreadCpuTime){
          remoteThreadBean.setThreadCpuTimeEnabled(false)
        }
        threadTimes
      }
    }
    val remoteThreads = remoteThreadBean.getThreadInfo(allThreadIds).toList/*.toArray.toList*/.map(ti => {
      JmxThreadSpec(ti.getThreadId,ti.getThreadName,ti.getThreadState,cpuTimes.get(ti.getThreadId))
    })
    val jmxResult = JmxResults(
      remoteOs,
      remoteRuntime,
      remoteMemory,
      remoteThreads
    )
    client.close
    val duration = new java.util.Date().getTime - start
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now.getTime,Map(
      "checkType" -> "jmx",
      "timeTaken" -> duration.toDouble,
      "loadAverage" -> remoteOs.loadAverage,
      "uptime" -> remoteRuntime.uptime,
      "startTime" -> remoteRuntime.startTime,
      "heapMax" -> remoteMemory.heap.max,
      "heapUsed" -> remoteMemory.heap.used,
      "nonHeapMax" -> remoteMemory.nonHeap.max,
      "nonHeapUsed" -> remoteMemory.nonHeap.used
    ))
    FunctionalCheckReturn(
      result = ScriptStepResult(jmxResult.toString,Map.empty[String,String],200,duration),
      duration = totalDuration + duration,
      updatedEnvironment = environment,
      jmxResults = Some(jmxResult),
      data = newData :: fcr.data
    )
  }
}

case class JmxOsSpec(name:String,arch:String,processorCount:Int,loadAverage:Double,version:String)
case class JmxRuntimeSpec(name:String,inputArgs:List[String],classPath:String,libraryPath:String,managementSpecVersion:String,specName:String,specVendor:String,specVersion:String,systemProperties:Map[String,String],uptime:Long,vmName:String,vmVendor:String,vmVersion:String,startTime:Long,bootClassPath:Option[String])
case class JmxMemoryUsage(init:Long,used:Long,committed:Long,max:Long)
case class JmxMemorySpec(heap:JmxMemoryUsage,nonHeap:JmxMemoryUsage,objectsPendingFinalizationCount:Int)
case class JmxThreadSpec(threadId:Long,name:String,threadState:java.lang.Thread.State,cpuTime:Option[Long])

case class JmxResults(os:JmxOsSpec,runtime:JmxRuntimeSpec,memory:JmxMemorySpec,threads:List[JmxThreadSpec])

abstract class JmxExtractingEnvironmentMutator extends FunctionalCheck {
  protected def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.jmxResults.map(jmxResult => fcr.copy(updatedEnvironment = mutate(jmxResult,environment,interpolator))).getOrElse(fcr)
  }
}

case class JmxOsLoadExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.loadAverage.toString)
  }
}
case class JmxOsProcessorCountExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.processorCount.toString)
  }
}
case class JmxOsNameExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.name)
  }
}
case class JmxOsArchExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.arch)
  }
}
case class JmxOsVersionExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.version)
  }
}

case class JmxRuntimeUptimeExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.runtime.uptime.toString)
  }
}

case class JmxRuntimeInputArgsExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.runtime.inputArgs.mkString(" "))
  }
}

case class JmxRuntimeNameExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.runtime.name)
  }
}
case class JmxHeapMemoryMaxExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.heap.max.toString)
  }
}
case class JmxHeapMemoryUsedExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.heap.used.toString)
  }
}
case class JmxHeapMemoryCommittedExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.heap.committed.toString)
  }
}
case class JmxHeapMemoryInitExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.heap.init.toString)
  }
}
case class JmxHeapMemoryPercentageExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),((result.memory.heap.used * 100) / result.memory.heap.max).toString)
  }
}
case class JmxNonHeapMemoryMaxExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.nonHeap.max.toString)
  }
}
case class JmxNonHeapMemoryUsedExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.nonHeap.used.toString)
  }
}
case class JmxNonHeapMemoryCommittedExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.nonHeap.committed.toString)
  }
}
case class JmxNonHeapMemoryInitExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.nonHeap.init.toString)
  }
}
case class JmxNonHeapMemoryPercentageExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),((result.memory.nonHeap.used * 100) / result.memory.nonHeap.max).toString)
  }
}

case class ResultValidator(description:String,validateResult:ScriptStepResult => Boolean) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    if (validateResult(previousResult)){
      fcr
    } else {
      throw new DashboardException("Result failed validation",environment.toString)
    }
  }
}

case class EnvironmentValidator(description:String,validateEnvironment:Map[String,String] => Boolean) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    if (validateEnvironment(environment)){
      fcr
    } else {
      throw new DashboardException("Environment failed validation",environment.toString)
    }
  }
}
abstract class EnvironmentMutator extends FunctionalCheck {
  protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.copy(updatedEnvironment = mutate(previousResult,environment,interpolator))
  }
}
case class LastDataExtractor(key:String,dataAttribute:String) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.data.headOption.flatMap(td => td._2.get(interpolator.interpolate(dataAttribute,environment))).map(nv => {
      fcr.copy(updatedEnvironment = environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(nv.toString,environment)))
    }).getOrElse(fcr)
  }
}
case class LatestDataExtractor(key:String,dataAttribute:String) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.data.flatMap(td => td._2.get(interpolator.interpolate(dataAttribute,fcr.updatedEnvironment)).map(da => (td._1,da.getAsString))).headOption.map(nv => {
      fcr.copy(updatedEnvironment = environment.updated(interpolator.interpolate(key,fcr.updatedEnvironment),interpolator.interpolate(nv._2.toString,fcr.updatedEnvironment)))
    }).getOrElse(fcr)
  }
}

abstract class HttpExtractingEnvironmentMutator extends FunctionalCheck {
  protected def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.httpResult.map(httpResult => fcr.copy(updatedEnvironment = mutate(httpResult,environment,interpolator))).getOrElse(fcr)
  }
}

case class HttpRequestUrlExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.requestUrl.toString,environment))
  }
}
case class HttpStatusCodeExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.statusCode.toString,environment))
  }
}

case class HttpHeaderExtractor(key:String,headerName:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    (for (
      headerValue <- result.headers.get(headerName)
    ) yield {
      environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(headerValue,environment))
    }).getOrElse(environment)
  }
}

case class HttpRedirectCountExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.numberOfRedirects.toString,environment))
  }
}

case class HttpRetryCountExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.numberOfRetries.toString,environment))
  }
}
case class HttpStartTimeExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.startMilis.toString,environment))
  }
}
case class HttpEndTimeExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.endMilis.toString,environment))
  }
}
case class HttpExceptionsExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  protected def stringifyExceptions(in:List[Exception]):String = in.toString
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.endMilis.toString,environment))
  }
}
abstract class SqlExtractingEnvironmentMutator extends FunctionalCheck with SafelyExtractFromSql {
  protected def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.sqlResult.map(sqlResult => fcr.copy(updatedEnvironment = mutate(sqlResult,environment,interpolator))).getOrElse(fcr)
  }
}
case class StoreSqlResultSet(key:String) extends SqlExtractingEnvironmentMutator {
  protected def safelyExtract(resultSet:SQLResultSet):String = resultSet.toString
  override def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(safelyExtract(result),environment))
  }
}

trait SafelyExtractFromSql {
  protected val defaultSeparator = ", "
  protected def safelyExtract(cell:SQLCell[_]):String = cell.value.toString
  protected def safelyExtract(row:SQLRow,separator:String):String = row.cells.values.toList.sortWith((a,b) => a.name < b.name).map(c => safelyExtract(c)).mkString(separator)
}

case class SqlRowExtractor(key:String,rowNumber:Int,separator:Option[String] = None) extends SqlExtractingEnvironmentMutator {
  override def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    (for(
      row <- result.rows.get(rowNumber)
    ) yield {
      environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(safelyExtract(row,separator.getOrElse(defaultSeparator)),environment))
    }).getOrElse(environment)
  }
}

case class SqlCellExtractor(key:String,rowNumber:Int,columnName:String) extends SqlExtractingEnvironmentMutator {
  override def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    (for(
      row <- result.rows.get(rowNumber);
      col <- row.cells.get(columnName)
    ) yield {
      environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(safelyExtract(col),environment))
    }).getOrElse(environment)
  }
}
case class SqlColExtractor(key:String,columnName:String,separator:Option[String] = None) extends SqlExtractingEnvironmentMutator {
  override def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    val combinedString = result.rows.values.toList.sortWith((a,b) => a.rowNumber < b.rowNumber).flatMap(r => r.cells.get(columnName).map(safelyExtract _)).mkString(separator.getOrElse(defaultSeparator))
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(combinedString,environment))
  }
}

trait SafelyExtractFromLdap {
  val defaultRecordSeparator = "\r\n"
  val defaultAttrSeparator = ", "
  val defaultAttrValueSeparator = " || " 
  def stringify(rs:LdapResults):String = rs.results.map(stringify _).mkString(defaultRecordSeparator)
  def stringify(r:LdapResult):String = "Record(%s,%s)".format(r.name,r.attrs.map(stringify _).mkString(defaultAttrSeparator))
  def stringify(r:LdapAttr):String = "Attribute(%s,%s)".format(r.name,r.values.mkString(defaultAttrValueSeparator))
}

abstract class LdapExtractingEnvironmentMutator extends FunctionalCheck with SafelyExtractFromLdap {
  protected def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.ldapResults.map(ldapResult => fcr.copy(updatedEnvironment = mutate(ldapResult,environment,interpolator))).getOrElse(fcr)
  }
}
case class StoreLdapResults(key:String) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(stringify(result),environment))
  }
}
case class LdapQueryExtractor(key:String) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.query,environment))
  }
}
case class LdapSearchBaseExtractor(key:String) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.base,environment))
  }
}
case class LdapRecordExtractor(key:String,recordName:String,separator:Option[String] = None) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    val resultString = result.results.filter(_.name == recordName).map(r => stringify(r)).mkString(separator.getOrElse(defaultRecordSeparator))
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(resultString,environment))
  }
}
case class LdapAttrExtractor(key:String,attrName:String,separator:Option[String] = None) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    val resultString = result.results.flatMap(_.attrs.filter(_.name == attrName)).map(a => stringify(a)).mkString(separator.getOrElse(defaultAttrSeparator))
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(resultString,environment))
  }
}
case class LdapAttrFromRecordExtractor(key:String,recordName:String,attrName:String,separator:Option[String] = None) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    (for (
      record <- result.results.find(_.name == recordName);
      attr <- record.attrs.find(_.name == attrName)
    ) yield {
      val attrString = attr.values.mkString(separator.getOrElse(defaultAttrValueSeparator))
      environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(attrString,environment))
    }).getOrElse(environment)
  }
}

abstract class ResultMutator extends FunctionalCheck {
  protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):ScriptStepResult
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.copy(result = mutate(previousResult,environment,interpolator))
  }
}

case class KeySetter(key:String,value:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(value,environment))
}

case class KeyDeleter(key:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = environment - interpolator.interpolate(key,environment)
}

case class ResultStorer(key:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = environment.updated(interpolator.interpolate(key,environment),result.body)
}
case class MetaDataStorer(key:String,headerName:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    result.metaData.get(headerName).map(newValue => environment.updated(interpolator.interpolate(key,environment),newValue)).getOrElse(environment)
  }
}
case class StatusCodeStorer(key:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.statusCode.toString)
  }
}

case class Cond(key:String,value:String,thenFuncs:List[FunctionalCheck],elseFuncs:List[FunctionalCheck]) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr)
    if (environment.get(interpolator.interpolate(key,environment)).exists(_ == interpolator.interpolate(value,environment))){
      thenFuncs.foreach(tf => {
        state.right.toOption.map(s => {
          state = tf.act(s,interpolator)
        })
      })
    } else {
      elseFuncs.foreach(tf => {
        state.right.toOption.map(s => {
          state = tf.act(s,interpolator)
        })
      })
    }
    state.left.map(e => throw e).right.toOption.get
  }
}

case class WhileLoop(key:String,value:String,funcs:List[FunctionalCheck]) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr)
    while (state.right.toOption.exists(s => s.updatedEnvironment.get(interpolator.interpolate(key,s.updatedEnvironment)).exists(_ == interpolator.interpolate(value,s.updatedEnvironment)))){
      funcs.foreach(tf => {
        state.right.toOption.map(s => {
          state = tf.act(s,interpolator)
        })
      })
    } 
    state.left.map(e => throw e).right.toOption.get
  }
}

case class ForLoop(key:String,start:Int,end:Int,incrementing:Boolean,funcs:List[FunctionalCheck]) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    var counter = start
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr.copy(updatedEnvironment = environment.updated(key,counter.toString)))
    if ((incrementing && start < end) || ((!incrementing) && start > end)){
      while (counter != end){
        state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment.updated(interpolator.interpolate(key,s.updatedEnvironment),counter.toString)))
        funcs.foreach(tf => {
          state.right.toOption.map(s => {
            state = tf.act(s,interpolator)
          })
        })
        if (incrementing){
          counter += 1
        } else {
          counter -= 1
        }
      } 
    }
    state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment - interpolator.interpolate(key,s.updatedEnvironment)))
    state.left.map(e => throw e).right.toOption.get
  }
}

case class ForeachRegexFromResult(key:String,regex:String,funcs:List[FunctionalCheck]) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val Pattern = interpolator.interpolate(regex,environment).r.unanchored
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr)
    previousResult.body match {
      case Pattern(matches @ _*) => {
        matches.foreach(m => {
          state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment.updated(interpolator.interpolate(key,s.updatedEnvironment),m)))
          funcs.foreach(tf => {
            state.right.toOption.map(s => {
              state = tf.act(s,interpolator)
            })
          })
        })
      }
      case _ => {}
    }
    state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment - interpolator.interpolate(key,s.updatedEnvironment)))
    state.left.map(e => throw e).right.toOption.get
  }
}

case class RegexFromResult(key:String,regex:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    val Pattern = interpolator.interpolate(regex,environment).r.unanchored
    var mutatedEnvironment = environment
    result.body match {
      case Pattern(matches @ _*) => {
        matches.headOption.foreach(firstMatch => {
          mutatedEnvironment = mutatedEnvironment.updated(interpolator.interpolate(key,mutatedEnvironment),firstMatch)
        })
        if (matches.length > 0){
          matches.zipWithIndex.foreach(m => {
            mutatedEnvironment = mutatedEnvironment.updated("%s_%s".format(interpolator.interpolate(key,mutatedEnvironment),m._2),m._1)
          })
        }
      }
      case Pattern(onlyMatch) => {
        mutatedEnvironment = mutatedEnvironment.updated(interpolator.interpolate(key,mutatedEnvironment),onlyMatch)
      }
      case other => {
        throw new DashboardException("Pattern didn't find a valid value: %s ".format(regex),other.toString)
      }
    }
    mutatedEnvironment
  }
}

case class Delay(delay:Long,randomize:Boolean = false) extends FunctionalCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val amount:Long = randomize match {
      case true => ((scala.util.Random.nextInt(200) * delay) / 100L)  // pick a value up to twice above the delay value, and down to zero.
      case false => delay
    }
    Thread.sleep(amount)
    fcr
  }
}


case class ForeachXPathFromResult(key:String,xPath:String,funcs:List[FunctionalCheck]) extends FunctionalCheck {
  import org.htmlcleaner._
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr)
    val cleaned = new HtmlCleaner().clean(previousResult.body)
    val matches = cleaned.evaluateXPath(interpolator.interpolate(xPath,environment)).toList.map(_.toString)
    matches.foreach(m => {
      state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment.updated(interpolator.interpolate(key,s.updatedEnvironment),m)))
      funcs.foreach(tf => {
        state.right.toOption.map(s => {
          state = tf.act(s,interpolator)
        })
      })
    })
    state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment - interpolator.interpolate(key,s.updatedEnvironment)))
    state.left.map(e => throw e).right.toOption.get
  }
}

case class XPathFromResult(key:String,xPath:String) extends EnvironmentMutator {
  import org.htmlcleaner._
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    var mutatedEnvironment = environment
    val cleaned = new HtmlCleaner().clean(result.body)
    val matches = cleaned.evaluateXPath(interpolator.interpolate(xPath,environment)).toList.map(_.toString)
    matches.headOption.foreach(firstMatch => {
      mutatedEnvironment = mutatedEnvironment.updated(interpolator.interpolate(key,mutatedEnvironment),firstMatch)
    })
    if (matches.length > 0){
      matches.zipWithIndex.foreach(m => {
        mutatedEnvironment = mutatedEnvironment.updated("%s_%s".format(interpolator.interpolate(key,mutatedEnvironment),m._2),m._1)
      })
    }
    mutatedEnvironment
  }
}

case class LiftFormExtractor(prefix:String) extends EnvironmentMutator {
  import org.htmlcleaner._
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment
  }
}

case class ResultSetter(seed:String) extends ResultMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):ScriptStepResult = ScriptStepResult(interpolator.interpolate(seed,environment))
}

case class RetrieveAttributeToResult(key:String) extends ResultMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):ScriptStepResult = environment.get(key).map(res => ScriptStepResult(interpolator.interpolate(res,environment))).getOrElse(result)
}

abstract class Interpolator {
  def interpolate(in:String,values:Map[String,String]):String
}
case object EmptyInterpolator extends Interpolator {
  override def interpolate(in:String,values:Map[String,String]):String = in
}
object Interpolator extends ConfigFileReader {
  def configureFromXml(n:Node):Option[Interpolator] = {
		getNodes(n,"interpolator").headOption.flatMap(mn => {
			getAttr(mn,"type").getOrElse("unknown") match {
				case "charKeyedStringInterpolator" => {
          (for (
            startTag <- getAttr(mn,"startTag");
            endTag <- getAttr(mn,"endTag")
          ) yield CharKeyedStringInterpolator(startTag,endTag))
        }
				case "escapedCharKeyedStringInterpolator" => {
          (for (
            startTag <- getAttr(mn,"startTag");
            endTag <- getAttr(mn,"endTag");
            escapeTag <- getAttr(mn,"escapeTag")
          ) yield EscapedCharKeyedStringInterpolator(startTag,endTag,escapeTag))
        }
				case _ => None
			}
		})
	}
}
case class CharKeyedStringInterpolator(startTag:String,endTag:String) extends Interpolator {
  protected val KeyStartTag = startTag
  protected val KeyEndTag = endTag
  def interpolate(in:String,values:Map[String,String]):String = {
    val (partialResult,possibleStartTag,possibleKey,possibleEndTag) = in.foldLeft(("","","",""))((acc,item) => {
      (acc,item) match {
        case ((soFar,KeyStartTag,key,partialEndTag),item) if partialEndTag + item == KeyEndTag => (soFar + values.get(key).getOrElse(KeyStartTag + key + partialEndTag + item),"","","") //end tag complete, commit interpolation
        case ((soFar,KeyStartTag,key,partialEndTag),item) if KeyEndTag.startsWith(partialEndTag + item) => (soFar,KeyStartTag,key,partialEndTag + item) //end tag still building
        case ((soFar,KeyStartTag,key,partialEndTag),item) => (soFar,KeyStartTag,key + partialEndTag + item,"") //start tag complete, key building
        case ((soFar,partialStartTag,key,""),item) if KeyStartTag.startsWith(partialStartTag + item) => (soFar,partialStartTag + item,"","") //start tag still building
        case ((soFar,partialStartTag,key,partialEndTag),item) => (soFar + partialStartTag + key + partialEndTag + item,"","","") //not matched, just adding to the base
      }
    })
    partialResult + possibleStartTag + possibleKey + possibleEndTag
  }
}
case class EscapedCharKeyedStringInterpolator(startTag:String,endTag:String,escapeString:String) extends Interpolator {
  protected val KeyStartTag = startTag
  protected val KeyEndTag = endTag
  protected val EscapeCharacters = escapeString
  def interpolate(in:String,values:Map[String,String]):String = {
    val (partialResult,possibleStartTag,possibleKey,possibleEndTag,escapeChars,escaping) = in.foldLeft(("","","","","",false))((acc,item) => {
      (acc,item) match {
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if escapePattern + item == EscapeCharacters => (soFar,KeyStartTag,key,partialEndTag,"",true) // escapePattern complete - set the next item to escapeMode
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if EscapeCharacters.startsWith(escapePattern + item) => (soFar,KeyStartTag,key,partialEndTag,escapePattern + item,false) // escapePattern building
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,true),item) => (soFar,KeyStartTag,key + partialEndTag + item,"","",false) //character escaped, continuing to build key
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if (partialEndTag + item).length > KeyEndTag.length => (soFar,KeyStartTag,key + partialEndTag + escapePattern + item,"","",false) //end tag became over-long, aborting end-tag and continuing to build key
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if (partialEndTag + escapePattern + item) == KeyEndTag => (soFar + values.get(key).getOrElse(KeyStartTag + key + partialEndTag + item),"","","","",false) //end tag complete, commit interpolation
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if KeyEndTag.startsWith(partialEndTag + escapePattern + item) => (soFar,KeyStartTag,key,partialEndTag + escapePattern + item,"",false) //end tag still building
        case ((soFar,partialStartTag,"","","",true),item) => (soFar + partialStartTag + item,"","","","",false) //character escaped, aborted building start tag
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) => (soFar,KeyStartTag,key + partialEndTag + escapePattern + item,"","",false) //start tag complete, key building
        case ((soFar,partialStartTag,key,"",escapePattern,false),item) if KeyStartTag == (partialStartTag + item) => (soFar,partialStartTag + escapePattern + item,"","","",false) //start tag complete
        case ((soFar,partialStartTag,key,"",escapePattern,false),item) if (partialStartTag + escapePattern + item).length > KeyStartTag.length => (soFar + partialStartTag + escapePattern + item,"","","","",false) //start tag became over-long, aborting start-tag and adding to base
        case ((soFar,partialStartTag,key,"",escapePattern,false),item) if KeyStartTag.startsWith(partialStartTag + item) => (soFar,partialStartTag + escapePattern + item,"","","",false) //start tag still building
        case ((soFar,partialStartTag,key,partialEndTag,escapePattern,false),item) => (soFar + partialStartTag + key + partialEndTag + escapePattern + item,"","","","",false) //not matched, just adding to the base
      }
    })
    partialResult + possibleStartTag + possibleKey + possibleEndTag + escapeChars + {escaping match {
      case true => escapeString
      case false => ""
    }}
  }
}

class ScriptExecutionEnvironment {
  lazy val httpClient:CleanHttpClient = Http.getClient
}


class ScriptEngine(interpolator:Interpolator) {
  def execute(sequence:List[FunctionalCheck]):FunctionalCheckReturn = {
    val see = new ScriptExecutionEnvironment()
    sequence.foldLeft(FunctionalCheckReturn.empty)((acc,i) => {
      i.attachScriptExecutionEnvironment(see)
      i.act(acc,interpolator) match {
        case Left(e) => {
          throw e
        }
        case Right(fcr) => {
          fcr
        }
      }
    })
  }
}
      
case class ScriptedCheck(serviceCheckMode:ServiceCheckMode,incomingLabel:String,sequence:List[FunctionalCheck],interpolator:Interpolator,time:TimeSpan) extends Pinger(incomingLabel,serviceCheckMode){
  override val pollInterval = time
  val scriptEngine = new ScriptEngine(interpolator)
  def status = {
    val fcr = scriptEngine.execute(sequence)
    val finalResult = fcr.result
    val totalDuration = fcr.duration
    val data = fcr.data
    (finalResult,Full(totalDuration),data)
  }
  override def performCheck = succeed(status._1.body,status._2,status._3)
}
