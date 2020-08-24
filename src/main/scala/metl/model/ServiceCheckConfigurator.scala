package metl.model

import metl.model.sensor._
import net.liftweb.common.{Empty, Full, Logger}
import net.liftweb.util.Helpers._

import scala.xml.Node
import net.liftweb.json._
import Serialization._

object ServiceCheckConfigurator extends ConfigFileReader with Logger {
  protected implicit val formats = DefaultFormats
  def configureFromJson(n: JValue): List[VisualElement] = {
    var failed: Boolean = false
    var errors: List[String] = Nil
    def getOrError[A](value: Option[A], default: A, error: String): A =
      value.getOrElse({
        failed = true
        errors = error :: errors
        default
      })

    n match {
      case ja @ JArray(items) => items.flatMap(configureFromJson _)
      case jo: JObject => {
        (for {
          serviceName <- (jo \ "serviceName").extractOpt[String]
          periodRaw = (jo \ "period").extractOpt[Int]
          serviceLabel = (jo \ "serviceLabel")
            .extractOpt[String]
            .getOrElse(serviceName)
          serverName <- (jo \ "serverName").extractOpt[String]
          serverLabel = (jo \ "serverLabel")
            .extractOpt[String]
            .getOrElse(serverName)
          id = getOrError((jo \ "id").extractOpt[String],
                          nextFuncName,
                          "id not supplied")
          serviceCheckName = (jo \ "name")
            .extractOpt[String]
            .getOrElse(id)
          label = (jo \ "label").extractOpt[String].getOrElse(serviceCheckName)
          mode = (jo \ "mode")
            .extractOpt[String]
            .map(ServiceCheckMode.parse _)
            .getOrElse(UNKNOWNMODE)
          severity = (jo \ "severity")
            .extractOpt[String]
            .map(ServiceCheckSeverity.parse _)
            .getOrElse(UNKNOWNSEVERITY)
          timeout = (jo \ "timeout").extractOpt[Int].map(t => new TimeSpan(t))
          acceptedFailures = (jo \ "requiredSequentialFailures").extractOpt[Int]
          expectFail = (jo \ "expectFail").extractOpt[Boolean].getOrElse(false)
          metadata = SensorMetaData(serviceCheckName,
                                    label,
                                    mode,
                                    severity,
                                    serviceName,
                                    serviceLabel,
                                    serverName,
                                    serverLabel,
                                    id,
                                    expectFail,
                                    timeout)
          sensorType <- (jo \ "type").extractOpt[String]
        } yield {
          lazy val period = getOrError(periodRaw.map(p => new TimeSpan(p)),
                                       new TimeSpan(300000),
                                       "no period supplied")
          val output = sensorType.trim.toLowerCase match {
            case "endpoint_information" => {
              val endpoints = (jo \ "endpoints")
                .extractOpt[JArray]
                .map(_.children)
                .getOrElse(Nil)
                .flatMap {
                  case epo: JObject =>
                    for {
                      u <- (epo \ "url").extractOpt[String]
                      n = (epo \ "name").extractOpt[String].getOrElse(u)
                      d = (epo \ "description").extractOpt[String].getOrElse("")
                    } yield {
                      EndpointDescriptor(n, u, d)
                    }
                  case _ => None
                }
                .toList
              val ed = (jo \ "html").extractOpt[String].getOrElse("")
              try {
                val html = scala.xml.XML.loadString(ed)
                Some(
                  new EndpointInformationWithHtml(metadata,
                                                  serviceCheckName,
                                                  html,
                                                  endpoints))
              } catch {
                case e: Throwable => {
                  Some(
                    EndpointInformationWithString(metadata,
                                                  serviceCheckName,
                                                  ed,
                                                  endpoints))
                }
              }
            }
            case "information" => {
              val htmlDescriptor =
                (jo \ "html").extractOpt[String].getOrElse("")
              try {
                val html = scala.xml.XML.loadString(htmlDescriptor)
                Some(HtmlInformation(metadata, serviceCheckName, html))
              } catch {
                case e: Throwable =>
                  Some(Information(metadata, serviceCheckName, htmlDescriptor))
              }
            }
            case "counting_mock" => {
              Some(CountingSensor(metadata, period))
            }
            case "waiting_mock" => {
              val minWait = (jo \ "minWait").extractOpt[Long].getOrElse(1000L)
              val additionalTime =
                (jo \ "variance").extractOpt[Int].getOrElse(2000)
              Some(WaitingSensor(metadata, minWait, additionalTime, period))
            }
            case "icmp" => {
              val host = getOrError((jo \ "host").extractOpt[String],
                                    "",
                                    "host not specified")
              val ipv6 = (jo \ "ipv6").extractOpt[Boolean].getOrElse(false)
              Some(PingICMPSensor(metadata, host, ipv6, period))
            }
            case "xmpp" => {
              val host =
                getOrError((jo \ "host").extractOpt[String],
                           "",
                           "host not specified")
              val domain = (jo \ "domain").extractOpt[String]
              val allowAnonymousAccess = false
              Some(
                XmppSensor(metadata,
                           host,
                           domain,
                           allowAnonymousAccess,
                           period))
            }
            case "oracle" => {
              val uri = getOrError((jo \ "connectionUri").extractOpt[String],
                                   "",
                                   "connection string not specified")
              val username = getOrError((jo \ "username").extractOpt[String],
                                        "",
                                        "username not specified")
              val password = getOrError((jo \ "password").extractOpt[String],
                                        "",
                                        "password not specified")
              val query =
                getOrError((jo \ "query").extractOpt[String],
                           "",
                           "query not specified")
              val thresholds = (jo \ "thresholds")
                .extractOpt[JArray]
                .toList
                .flatMap(_.children)
                .map(ts => {
                  val rowBehaviour =
                    (jo \ "rows").extractOpt[String].getOrElse("all")
                  val tsMap = Matchers.configureFromJson(ts)
                  VerifiableSqlResultSetDefinition(rowBehaviour, tsMap)
                })
              Some(
                OracleSensor(metadata,
                             uri,
                             username,
                             password,
                             query,
                             thresholds,
                             period))
            }
            case "sql" => {
              val url =
                getOrError((jo \ "url").extractOpt[String],
                           "",
                           "url not specified")
              val username = (jo \ "username").extractOpt[String]
              val password = (jo \ "password").extractOpt[String]
              val driver =
                getOrError((jo \ "driver").extractOpt[String],
                           "",
                           "driver not specified")
              val additionalProps = (jo \ "additionalProps")
                .extractOpt[JValue]
                .toList
                .flatMap(_.children)
                .flatMap(ap => {
                  for {
                    k <- (ap \ "key").extractOpt[String]
                    v <- (ap \ "value").extractOpt[String]
                  } yield {
                    (k, v)
                  }
                })
              val query =
                getOrError((jo \ "query").extractOpt[String],
                           "",
                           "query not specified")
              val thresholds = (jo \ "thresholds")
                .extractOpt[JArray]
                .toList
                .flatMap(_.children)
                .map(ts => {
                  val rowBehaviour =
                    (jo \ "rows").extractOpt[String].getOrElse("all")
                  val tsMap = Matchers.configureFromJson(ts)
                  VerifiableSqlResultSetDefinition(rowBehaviour, tsMap)
                })
              Some(
                SQLSensor(metadata,
                          driver,
                          url,
                          query,
                          username,
                          password,
                          additionalProps,
                          thresholds,
                          period))
            }

            case "mysql" => {
              val host =
                getOrError((jo \ "host").extractOpt[String],
                           "",
                           "host not specified")
              val username = getOrError((jo \ "username").extractOpt[String],
                                        "",
                                        "no username supplied")
              val password = getOrError((jo \ "password").extractOpt[String],
                                        "",
                                        "no username supplied")
              val db =
                getOrError((jo \ "db").extractOpt[String],
                           "",
                           "db not specified")
              val query =
                getOrError((jo \ "query").extractOpt[String],
                           "",
                           "query not specified")
              val thresholds = (jo \ "thresholds")
                .extractOpt[JArray]
                .toList
                .flatMap(_.children)
                .map(ts => {
                  val rowBehaviour =
                    (jo \ "rows").extractOpt[String].getOrElse("all")
                  val tsMap = Matchers.configureFromJson(ts)
                  VerifiableSqlResultSetDefinition(rowBehaviour, tsMap)
                })
              Some(
                MySQLSensor(metadata,
                            host,
                            db,
                            query,
                            username,
                            password,
                            thresholds,
                            period))
            }
            case "mongo" => {
              val host =
                getOrError((jo \ "host").extractOpt[String],
                           "",
                           "host not specified")
              val port =
                getOrError((jo \ "port").extractOpt[Int],
                           -1,
                           "port not specified")
              val db = getOrError((jo \ "database").extractOpt[String],
                                  "",
                                  "database not specified")
              val table =
                getOrError((jo \ "table").extractOpt[String],
                           "",
                           "table not specified")
              Some(PingMongo(metadata, host, port, db, table, period))
            }
            case "memcached" => {
              val host =
                getOrError((jo \ "host").extractOpt[String],
                           "",
                           "host not specified")
              Some(PingMemCached(metadata, host, period))
            }
            case "ldap" => {
              val host =
                getOrError((jo \ "host").extractOpt[String],
                           "",
                           "host not specified")
              val username = getOrError((jo \ "username").extractOpt[String],
                                        "",
                                        "username not specified")
              val password = getOrError((jo \ "password").extractOpt[String],
                                        "",
                                        "password not specified")
              val searchTerm = getOrError(
                (jo \ "searchTerm").extractOpt[String],
                "",
                "searchTerm not specified")
              val searchBase = getOrError(
                (jo \ "searchBase").extractOpt[String],
                "",
                "searchTerm not specified")
              Some(
                new LdapSensor(metadata,
                               host,
                               username,
                               password,
                               searchBase,
                               searchTerm,
                               period))
            }
            case "munin" => {
              val host = getOrError((jo \ "host").extractOpt[String],
                                    "localhost",
                                    "host not specified")
              val port =
                (jo \ "port").extractOpt[Int].getOrElse(4949)
              Some(
                new MuninSensor(
                  metadata,
                  host,
                  port,
                  List(MuninCategoryDefinition("cpu", PercentageCounter),
                       MuninCategoryDefinition("memory", Guage)),
                  period))
            }
            case "munin_threshold" => {
              val host = getOrError((jo \ "host").extractOpt[String],
                                    "localhost",
                                    "host not specified")
              val port =
                (jo \ "port").extractOpt[Int].getOrElse(4949)
              val thresholds = Map(
                (jo \ "thresholds")
                  .extractOpt[JArray]
                  .toList
                  .flatMap(_.children)
                  .flatMap(ts => {
                    for {
                      tsName <- (jo \ "name").extractOpt[String]
                      tsType = (jo \ "type")
                        .extractOpt[String]
                        .getOrElse("counter")
                    } yield {
                      val tsMap = Matchers.configureFromJson(ts)
                      (tsName,
                       MuninCategoryDefinition(tsName,
                                               MuninFieldType.parse(tsType),
                                               tsMap))
                    }
                  }): _*)
              Some(
                MuninSensorAgainstThreshhold(metadata,
                                             host,
                                             port,
                                             thresholds,
                                             period))
            }
            case "samba" => {
              val host = getOrError((jo \ "host").extractOpt[String],
                                    "localhost",
                                    "host not specified")
              val domain = getOrError((jo \ "domain").extractOpt[String],
                                      "workgroup",
                                      "domain not specified")
              val file = getOrError((jo \ "file").extractOpt[String],
                                    "serverStatus",
                                    "file not specified")
              val username =
                getOrError((jo \ "username").extractOpt[String],
                           "",
                           "username not specified")
              val password =
                getOrError((jo \ "password").extractOpt[String],
                           "",
                           "password not specified")
              Some(
                SambaSensor(metadata,
                            host,
                            domain,
                            file,
                            username,
                            password,
                            period))
            }
            case "svn" => {
              val host =
                getOrError((jo \ "host").extractOpt[String],
                           "",
                           "host not specified")
              val username = getOrError((jo \ "username").extractOpt[String],
                                        "",
                                        "username not specified")
              val password = getOrError((jo \ "password").extractOpt[String],
                                        "",
                                        "password not specified")
              Some(SvnSensor(metadata, host, username, password, period))
            }
            case "http" => {
              val url =
                getOrError((jo \ "url").extractOpt[String],
                           "",
                           "url not specified")
              val matcher: HTTPResponseMatcher =
                HTTPResponseMatchers.configureFromJson((jo \ "thresholds"))
              val additionalHeaders = (jo \ "headers")
                .extractOpt[JArray]
                .toList
                .flatMap(_.children)
                .flatMap(hv => {
                  for {
                    k <- (hv \ "name").extractOpt[String]
                    v <- (hv \ "value").extractOpt[String]
                    if k.length > 0 && v.length > 0
                  } yield {
                    (k, v)
                  }
                })
              Some(
                HttpSensor(metadata, url, additionalHeaders, matcher, period))
            }
            case "http_with_credentials" => {
              val url =
                getOrError((jo \ "url").extractOpt[String],
                           "",
                           "url not specified")
              val username =
                getOrError((jo \ "username").extractOpt[String],
                           "",
                           "username not specified")
              val password =
                getOrError((jo \ "password").extractOpt[String],
                           "",
                           "password not specified")
              val matcher: HTTPResponseMatcher =
                HTTPResponseMatchers.configureFromJson((jo \ "thresholds"))
              val additionalHeaders = (jo \ "headers")
                .extractOpt[JArray]
                .toList
                .flatMap(_.children)
                .flatMap(hv => {
                  for {
                    k <- (hv \ "name").extractOpt[String]
                    v <- (hv \ "value").extractOpt[String]
                    if k.length > 0 && v.length > 0
                  } yield {
                    (k, v)
                  }
                })
              Some(
                HttpSensorWithBasicAuth(metadata,
                                        url,
                                        username,
                                        password,
                                        additionalHeaders,
                                        matcher,
                                        period))
            }
            case "dependency" => {
              val matchers: Map[DependencyDescription, DependencyMatcher] =
                Map(
                  (jo \ "thresholds")
                    .extractOpt[JValue]
                    .toList
                    .flatMap(_.children)
                    .map(t => {
                      val pinger =
                        (t \ "check").extractOpt[String].getOrElse("unknown")
                      val service = (t \ "service").extractOpt[String]
                      val server = (t \ "server").extractOpt[String]
                      val serviceCheckMode = (t \ "serviceCheckMode")
                        .extractOpt[String]
                        .map(scm => ServiceCheckMode.parse(scm))
                      val desc = DependencyDescription(pinger,
                                                       server,
                                                       service,
                                                       serviceCheckMode)
                      val matcher = DependencyMatchers.configureFromJson(t)
                      (desc, matcher)
                    }): _*)
              Some(DependencySensor(metadata, matchers, period))
            }
            case "matcher" => {
              (jo \ "matcher")
                .extractOpt[JObject]
                .flatMap(m => {
                  Matchers
                    .configureFromJson(m)
                    .headOption
                    .map(ho => {
                      MatcherCheck(metadata, ho._2, period)
                    })
                })
            }
            case "script" => {
              val interpolator =
                Interpolator.configureFromJson((jo \ "interpolator"))
              val sequence =
                FunctionalServiceCheck.configureFromJson((jo \ "steps"))
              Some(
                ScriptedSensor(metadata,
                               sequence,
                               interpolator.getOrElse(EmptyInterpolator),
                               period))
            }
            case _ => None
          }
          failed match {
            case true =>
              Some(
                ErrorInformation(
                  metadata,
                  "serviceCheck couldn't be constructed from arguments",
                  period.toString,
                  jo.toString,
                  errors))
            case false => output
          }
        }).flatten.toList
      }
    }
  }

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
        val id = getText(sc, "id").getOrElse(nextFuncName)
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
                                      id,
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
                                          "domain not specified")
                  val file = getOrError(getText(sc, "file"),
                                        "serverStatus",
                                        "file not specified")
                  val username = getOrError(getText(sc, "username"),
                                            "",
                                            "username not specified")
                  val password = getOrError(getText(sc, "password"),
                                            "",
                                            "password not specified")
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
