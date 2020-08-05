package metl.model

import metl.model.sensor._
import net.liftweb.common.{Empty, Full, Logger}
import net.liftweb.util.Helpers._

import scala.xml.Node

object ServiceCheckConfigurator extends ConfigFileReader with Logger {
  def configureFromXml(xml: Node,
                       serviceName: String = "unknown",
                       serviceLabel: String = "unknown",
                       serverName: String = "unknown",
                       serverLabel: String = "unknown"): List[VisualElement] = {
    trace("loading xml: %s".format(xml))
    (xml \\ "serviceCheck")
      .map(sc => {
        debug("Configuring serviceCheck from:\n%s".format(sc))
        val periodInt = (getLong(sc, "period").getOrElse(60L)) * 1000
        val period = new TimeSpan(periodInt)
        val serviceCheckName = getAttr(sc, "name").getOrElse("unnamed")
        val label = getText(sc, "label").getOrElse("no label")
        val mode = ServiceCheckMode.parse(getText(sc, "mode").getOrElse("test"))
        val severity =
          ServiceCheckSeverity.parse(getText(sc, "severity").getOrElse("alert"))
        val timeout = getInt(sc, "timeout").map(t => new TimeSpan(t))
        val acceptedFailures = getInt(sc, "requiredSequentialFailures")
        val expectFail = getBool(sc, "expectFail")
        var failed = false
        var errors = List.empty[String]
        val metadata = SensorMetaData(serviceCheckName,
                                      label,
                                      mode,
                                      severity,
                                      serviceName,
                                      serviceLabel,
                                      serverName,
                                      serverLabel,
                                      expectFail.getOrElse(false),
                                      timeout)
        def getOrError[A](value: Option[A], default: A, error: String): A =
          value.getOrElse({
            failed = true
            errors = error :: errors
            default
          })
        val output: VisualElement = try {
          getText(sc, "type")
            .map(typeString =>
              typeString.toLowerCase.trim match {
                case "endpoint_information" => {
                  val endpoints = getNodes(sc, "endpoint")
                    .map(epxml => {
                      val epName = getText(epxml, "name").getOrElse("unnamed")
                      val epUrl = getText(epxml, "url").getOrElse("")
                      val epDesc = getText(epxml, "description").getOrElse("")
                      EndpointDescriptor(epName, epUrl, epDesc)
                    })
                    .toList
                  val htmlDescriptor = getText(sc, "html").getOrElse("")
                  try {
                    val html = scala.xml.XML.loadString(htmlDescriptor)
                    new EndpointInformationWithHtml(metadata,
                                                    serviceCheckName,
                                                    html,
                                                    endpoints)
                  } catch {
                    case e: Throwable =>
                      EndpointInformationWithString(metadata,
                                                    serviceCheckName,
                                                    htmlDescriptor,
                                                    endpoints)
                  }
                }
                case "information" => {
                  val htmlDescriptor = getText(sc, "html").getOrElse("")
                  try {
                    val html = scala.xml.XML.loadString(htmlDescriptor)
                    HtmlInformation(metadata, serviceCheckName, html)
                  } catch {
                    case e: Throwable =>
                      Information(metadata, serviceCheckName, htmlDescriptor)
                  }
                }
                case "counting_mock" => {
                  CountingSensor(metadata, period)
                }
                case "waiting_mock" => {
                  val minWait = getLong(sc, "minWait").getOrElse(1000L)
                  val additionalTime = getInt(sc, "variance").getOrElse(2000)
                  WaitingSensor(metadata, minWait, additionalTime, period)
                }
                case "icmp" => {
                  val host =
                    getOrError(getText(sc, "host"), "", "host not specified")
                  val ipv6 = getBool(sc, "ipv6").getOrElse(false)
                  PingICMPSensor(metadata, host, ipv6, period)
                }
                case "xmpp" => {
                  val host =
                    getOrError(getText(sc, "host"), "", "host not specified")
                  val domain = getText(sc, "domain") match {
                    case Some(d) => Full(d)
                    case _       => Empty
                  }
                  val allowAnonymousAccess = false
                  XmppSensor(metadata,
                             host,
                             domain,
                             allowAnonymousAccess,
                             period)
                }
                case "oracle" => {
                  val uri = getOrError(getText(sc, "connectionUri"),
                                       "",
                                       "connection string not specified")
                  val username = getOrError(getText(sc, "username"),
                                            "",
                                            "username not specified")
                  val password = getOrError(getText(sc, "password"),
                                            "",
                                            "password not specified")
                  val query =
                    getOrError(getText(sc, "query"), "", "query not specified")
                  val thresholds = getNodes(sc, "thresholds").map(ts => {
                    val rowBehaviour = getAttr(ts, "rows").getOrElse("all")
                    val tsMap = Matchers.configureFromXml(ts)
                    VerifiableSqlResultSetDefinition(rowBehaviour, tsMap)
                  })
                  OracleSensor(metadata,
                               uri,
                               username,
                               password,
                               query,
                               thresholds,
                               period)
                }
                case "sql" => {
                  val url =
                    getOrError(getText(sc, "url"), "", "url not specified")
                  val username = getText(sc, "username")
                  val password = getText(sc, "password")
                  val driver = getOrError(getText(sc, "driver"),
                                          "",
                                          "driver not specified")
                  val additionalProps = for {
                    prop <- (sc \ "additionalProp").toList
                    k <- (prop \ "@key").headOption.map(_.text)
                    v <- prop.headOption.map(_.text)
                  } yield {
                    (k, v)
                  }
                  val query =
                    getOrError(getText(sc, "query"), "", "query not specified")
                  val thresholds = getNodes(sc, "thresholds").map(ts => {
                    val rowBehaviour = getAttr(ts, "rows").getOrElse("all")
                    val tsMap = Matchers.configureFromXml(ts)
                    VerifiableSqlResultSetDefinition(rowBehaviour, tsMap)
                  })
                  SQLSensor(metadata,
                            driver,
                            url,
                            query,
                            username,
                            password,
                            additionalProps,
                            thresholds,
                            period)
                }

                case "mysql" => {
                  val host =
                    getOrError(getText(sc, "host"), "", "host not specified")
                  val username = getOrError(getText(sc, "username"),
                                            "",
                                            "username not specified")
                  val password = getOrError(getText(sc, "password"),
                                            "",
                                            "password not specified")
                  val db = getOrError(getText(sc, "database"),
                                      "",
                                      "database not specified")
                  val query =
                    getOrError(getText(sc, "query"), "", "query not specified")
                  val thresholds = getNodes(sc, "thresholds").map(ts => {
                    val rowBehaviour = getAttr(ts, "rows").getOrElse("all")
                    val tsMap = Matchers.configureFromXml(ts)
                    VerifiableSqlResultSetDefinition(rowBehaviour, tsMap)
                  })
                  MySQLSensor(metadata,
                              host,
                              db,
                              query,
                              username,
                              password,
                              thresholds,
                              period)
                }
                case "mongo" => {
                  val host =
                    getOrError(getText(sc, "host"), "", "host not specified")
                  val port =
                    getOrError(getInt(sc, "port"), -1, "port not specified")
                  val db = getOrError(getText(sc, "database"),
                                      "",
                                      "database not specified")
                  val table =
                    getOrError(getText(sc, "table"), "", "table not specified")
                  PingMongo(metadata, host, port, db, table, period)
                }
                case "memcached" => {
                  val host =
                    getOrError(getText(sc, "host"), "", "host not specified")
                  PingMemCached(metadata, host, period)
                }
                case "ldap" => {
                  val host =
                    getOrError(getText(sc, "host"), "", "host not specified")
                  val username = getOrError(getText(sc, "username"),
                                            "",
                                            "username not specified")
                  val password = getOrError(getText(sc, "password"),
                                            "",
                                            "password not specified")
                  val searchTerm = getOrError(getText(sc, "searchTerm"),
                                              "",
                                              "searchTerm not specified")
                  val searchBase = getOrError(getText(sc, "searchBase"),
                                              "",
                                              "searchTerm not specified")
                  new LdapSensor(metadata,
                                 host,
                                 username,
                                 password,
                                 searchBase,
                                 searchTerm,
                                 period)
                }
                case "munin" => {
                  val host = getOrError(getText(sc, "host"),
                                        "localhost",
                                        "host not specified")
                  val port =
                    getOrError(getInt(sc, "port"), 4949, "port not specified")
                  new MuninSensor(
                    metadata,
                    host,
                    port,
                    List(MuninCategoryDefinition("cpu", PercentageCounter),
                         MuninCategoryDefinition("memory", Guage)),
                    period)
                }
                case "munin_threshold" => {
                  val host = getOrError(getText(sc, "host"),
                                        "localhost",
                                        "host not specified")
                  val port =
                    getOrError(getInt(sc, "port"), 4949, "port not specified")
                  val thresholds = Map(getNodes(sc, "thresholds").map(ts => {
                    val tsName = getAttr(ts, "name").getOrElse("unknown")
                    val tsType = getAttr(ts, "type").getOrElse("counter")
                    val tsMap = Matchers.configureFromXml(ts)
                    (tsName,
                     MuninCategoryDefinition(tsName,
                                             MuninFieldType.parse(tsType),
                                             tsMap))
                  }): _*)
                  MuninSensorAgainstThreshhold(metadata,
                                               host,
                                               port,
                                               thresholds,
                                               period)
                }
                case "samba" => {
                  val host = getOrError(getText(sc, "host"),
                                        "localhost",
                                        "host not specified")
                  val domain = getOrError(getText(sc, "domain"),
                                          "workgroup",
                                          "host not specified")
                  val file = getOrError(getText(sc, "file"),
                                        "serverStatus",
                                        "host not specified")
                  val username = getOrError(getText(sc, "username"),
                                            "",
                                            "host not specified")
                  val password = getOrError(getText(sc, "password"),
                                            "",
                                            "host not specified")
                  SambaSensor(metadata,
                              host,
                              domain,
                              file,
                              username,
                              password,
                              period)
                }
                case "svn" => {
                  val host =
                    getOrError(getText(sc, "host"), "", "host not specified")
                  val username = getOrError(getText(sc, "username"),
                                            "",
                                            "username not specified")
                  val password = getOrError(getText(sc, "password"),
                                            "",
                                            "password not specified")
                  SvnSensor(metadata, host, username, password, period)
                }
                case "http" => {
                  val url =
                    getOrError(getText(sc, "url"), "", "url not specified")
                  val matcher: HTTPResponseMatcher =
                    HTTPResponseMatchers.configureFromXml(
                      <thresholdsPacket>{getNodes(sc,"thresholds")}</thresholdsPacket>)
                  val additionalHeaders = getNodes(sc, "header")
                    .map(c => {
                      (getText(c, "name").getOrElse(""),
                       getText(c, "value").getOrElse(""))
                    })
                    .filter(h => {
                      h match {
                        case (n: String, v: String)
                            if n.length > 0 && v.length > 0 =>
                          true
                        case _ => false
                      }
                    })
                    .toList
                  HttpSensor(metadata, url, additionalHeaders, matcher, period)
                }
                case "http_with_credentials" => {
                  val url =
                    getOrError(getText(sc, "url"), "", "url not specified")
                  val username = getOrError(getText(sc, "username"),
                                            "",
                                            "username not specified")
                  val password = getOrError(getText(sc, "password"),
                                            "",
                                            "password not specified")
                  val matcher: HTTPResponseMatcher =
                    HTTPResponseMatchers.configureFromXml(
                      <thresholdsPacket>{getNodes(sc,"thresholds")}</thresholdsPacket>)
                  val additionalHeaders = getNodes(sc, "header")
                    .map(c => {
                      (getText(c, "name").getOrElse(""),
                       getText(c, "value").getOrElse(""))
                    })
                    .filter(h => {
                      h match {
                        case (n: String, v: String)
                            if n.length > 0 && v.length > 0 =>
                          true
                        case _ => false
                      }
                    })
                    .toList
                  HttpSensorWithBasicAuth(metadata,
                                          url,
                                          username,
                                          password,
                                          additionalHeaders,
                                          matcher,
                                          period)
                }
                case "dependency" => {
                  val matchers: Map[DependencyDescription, DependencyMatcher] =
                    Map(getNodes(sc, "thresholds").map(t => {
                      val pinger = getAttr(t, "check").getOrElse("unknown")
                      val service = getAttr(t, "service")
                      val server = getAttr(t, "server")
                      val serviceCheckMode = getAttr(t, "serviceCheckMode").map(
                        scm => ServiceCheckMode.parse(scm))
                      val desc = DependencyDescription(pinger,
                                                       server,
                                                       service,
                                                       serviceCheckMode)
                      val matcher = DependencyMatchers.configureFromXml(t)
                      (desc, matcher)
                    }): _*)
                  DependencySensor(metadata, matchers, period)
                }
                case "matcher" => {
                  getImmediateNodes(sc, "matcher")
                    .map(mNodes => {
                      Matchers
                        .configureFromXml(mNodes)
                        .headOption
                        .map(ho => {
                          val matcher = ho._2
                          MatcherCheck(metadata, matcher, period)
                        })
                        .getOrElse(NullCheck)
                    })
                    .headOption
                    .getOrElse(NullCheck)
                }
                case "script" => {
                  val interpolator = Interpolator.configureFromXml(
                    <interpolators>{getImmediateNodes(sc,"interpolator")}</interpolators>)
                  val sequence = FunctionalServiceCheck.configureFromXml(
                    <steps>{getImmediateNodes(sc,"step")}</steps>)
                  ScriptedSensor(metadata,
                                 sequence,
                                 interpolator.getOrElse(EmptyInterpolator),
                                 period)
                }
                case other => {
                  failed = true
                  errors = "unknown type" :: errors
                  NullCheck
                }
            })
            .getOrElse({
              failed = true
              errors = "no type specified" :: errors
              NullCheck
            })
        } catch {
          case e: Throwable => {
            errors = e.getMessage :: errors
            NullCheck
          }
        }
        val result =
          if (failed)
            ErrorInformation(metadata,
                             "serviceCheck couldn't be constructed from xml",
                             period.toString,
                             sc.toString,
                             errors)
          else output
        result
      })
      .toList
  }
}
