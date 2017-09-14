package metl.model

import net.liftweb.common.{Empty, Full}
import net.liftweb.util.Helpers._

import scala.xml.Node

object ServiceCheckConfigurator extends ConfigFileReader {
  def configureFromXml(xml:Node, serviceName:String = "unknown", serverName:String = "unknown"):List[VisualElement] = {
    (xml \\ "serviceCheck").map(sc => {
      debug("Configuring serviceCheck from:\n%s".format(sc))
      val periodInt = getInt(sc,"period").getOrElse(60) * 1000
      val period = new TimeSpan(periodInt)
      val serviceCheckName = getAttr(sc,"name").getOrElse("unnamed")
      val label = getText(sc,"label").getOrElse("no label")
      val mode = ServiceCheckMode.parse(getText(sc,"mode").getOrElse("test"))
      val timeout = getInt(sc,"timeout").map(t => new TimeSpan(t))
      val acceptedFailures = getInt(sc,"requiredSequentialFailures")
      val expectFail = getBool(sc,"expectFail")
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
            try {
              val html = scala.xml.XML.loadString(htmlDescriptor)
              new EndpointInformationWithHtml(serviceCheckName,html,endpoints)
            } catch {
              case e:Throwable =>
                EndpointInformationWithString(serviceCheckName,htmlDescriptor,endpoints)
            }
          }
          case "information" => {
            val htmlDescriptor = getText(sc,"html").getOrElse("")
            try {
              val html = scala.xml.XML.loadString(htmlDescriptor)
              HtmlInformation(serviceCheckName,html)
            } catch {
              case e:Throwable =>
                Information(serviceCheckName,htmlDescriptor)
            }
          }
          case "icmp" => {
            val host = getOrError(getText(sc,"host"),"","host not specified")
            val ipv6 = getBool(sc,"ipv6").getOrElse(false)
            PingICMP(mode,serviceCheckName,label,host,ipv6,period)
          }
          case "xmpp" => {
            val host = getOrError(getText(sc,"host"),"","host not specified")
            val domain = getText(sc,"domain") match {
              case Some(d) => Full(d)
              case _ => Empty
            }
            val allowAnonymousAccess = false
            PingXmpp(mode,serviceCheckName,label,host,domain,allowAnonymousAccess,period)
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
            PingOracle(mode,serviceCheckName,label,uri,username,password,query,thresholds,period)
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
            PingMySQL(mode,serviceCheckName,label,host,db,query,username,password,thresholds,period)
          }
          case "mongo" => {
            val host = getOrError(getText(sc,"host"),"","host not specified")
            val port = getOrError(getInt(sc,"port"),-1,"port not specified")
            val db = getOrError(getText(sc,"database"),"","database not specified")
            val table = getOrError(getText(sc,"table"),"","table not specified")
            PingMongo(mode,serviceCheckName,label,host,port,db,table,period)
          }
          case "memcached" => {
            val host = getOrError(getText(sc,"host"),"","host not specified")
            PingMemCached(mode,serviceCheckName,label,host,period)
          }
          case "ldap" => {
            val host = getOrError(getText(sc,"host"),"","host not specified")
            val username = getOrError(getText(sc,"username"),"","username not specified")
            val password = getOrError(getText(sc,"password"),"","password not specified")
            val searchTerm = getOrError(getText(sc,"searchTerm"),"","searchTerm not specified")
            val searchBase = getOrError(getText(sc,"searchBase"),"","searchTerm not specified")
            new PingLDAP(mode,serviceCheckName,label,host,username,password,searchBase,searchTerm,period)
          }
          case "munin" => {
            val host = getOrError(getText(sc,"host"),"localhost","host not specified")
            val port = getOrError(getInt(sc,"port"),4949,"port not specified")
            new PingMunin(mode,serviceCheckName,label,host,port,List(MuninCategoryDefinition("cpu",PercentageCounter),MuninCategoryDefinition("memory",Guage)),period)
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
            PingMuninAgainstThreshhold(mode,serviceCheckName,label,host,port,thresholds,period)
          }
          case "samba" => {
            val host = getOrError(getText(sc,"host"),"localhost","host not specified")
            val domain = getOrError(getText(sc,"domain"),"workgroup","host not specified")
            val file = getOrError(getText(sc,"file"),"serverStatus","host not specified")
            val username = getOrError(getText(sc,"username"),"","host not specified")
            val password = getOrError(getText(sc,"password"),"","host not specified")
            PingSamba(mode,serviceCheckName,label,host,domain,file,username,password,period)
          }
          case "svn" => {
            val host = getOrError(getText(sc,"host"),"","host not specified")
            val username = getOrError(getText(sc,"username"),"","username not specified")
            val password = getOrError(getText(sc,"password"),"","password not specified")
            PingSVN(mode,serviceCheckName,label,host,username,password,period)
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
            HttpCheck(mode,serviceCheckName,label,url,additionalHeaders,matcher,period)
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
            HttpCheckWithBasicAuth(mode,serviceCheckName,label,url,username,password,additionalHeaders,matcher,period)
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
            DependencyCheck(mode,serviceCheckName,label,matchers,period)
          }
          case "matcher" => {
            getImmediateNodes(sc,"matcher").map(mNodes => {
              Matchers.configureFromXml(mNodes).headOption.map(ho => {
                val matcher = ho._2
                MatcherCheck(mode,serviceCheckName,label,matcher,period)
              }).getOrElse(NullCheck)
            }).headOption.getOrElse(NullCheck)
          }
          case "script" => {
            val interpolator = Interpolator.configureFromXml( <interpolators>{getImmediateNodes(sc,"interpolator")}</interpolators> )
            val sequence = FunctionalServiceCheck.configureFromXml( <steps>{getImmediateNodes(sc,"step")}</steps> )
            ScriptedCheck(mode,serviceCheckName,label,sequence,interpolator.getOrElse(EmptyInterpolator),period)
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

