package metl.model.sensor

import metl.model.{ConfigFileReader, Matchers, JsonReader}
import net.liftweb.common.Logger

import scala.xml.Node
import net.liftweb.json._

object FunctionalServiceCheck extends ConfigFileReader with JsonReader {
  def configureFromJson(v: JValue): List[FunctionalServiceCheck] = {
    asArrayOfObjs(v \ "step").flatMap(mn => {
      asString(mn \ "type") match {
        case Some("http") => {
          for {
            url <- asString(mn \ "url")
            method <- asString(mn \ "method")
            params = asArrayOfObjs(mn \ "parameter").flatMap(pn => {
              for {
                key <- asString(pn \ "key")
                value <- asString(pn \ "value")
              } yield {
                (key, value)
              }
            })
            headers = Map(asArrayOfObjs(mn \ "header").flatMap(hn => {
              for {
                key <- asString(hn \ "key")
                value <- asString(hn \ "value")
              } yield {
                (key, value)
              }
            }): _*)
            matcher: HTTPResponseMatcher = HTTPResponseMatchers
              .configureFromJson(mn \ "thresholds")
          } yield HttpFunctionalCheck(method, url, params, headers, matcher)
        }
        case Some("sql") => {
          for {
            driver <- asString(mn \ "driver")
            url <- asString(mn \ "url")
            username <- asString(mn \ "username")
            password <- asString(mn \ "password")
            query <- asString(mn \ "query")
            connectionTimeout = (mn \ "connectionTimeout").extractOpt[Long]
            matchers: List[VerifiableSqlResultSetDefinition] = asArrayOfObjs(
              mn \ "thresholds").map(ts => {
              val rowBehaviour = asString(ts \ "rows").getOrElse("all")
              val tsMap = Matchers.configureFromJson(ts)
              VerifiableSqlResultSetDefinition(rowBehaviour, tsMap)
            })
          } yield {
            JDBCFunctionalCheck(driver,
                                url,
                                username,
                                password,
                                query,
                                matchers,
                                connectionTimeout.getOrElse(10000L))
          }
        }
        case Some("ldap") => {
          for {
            host <- asString(mn \ "url")
            username <- asString(mn \ "username")
            password <- asString(mn \ "password")
            searchBase <- asString(mn \ "searchBase")
            query <- asString(mn \ "query")
          } yield {
            LdapFunctionalCheck(host, username, password, searchBase, query)
          }
        }
        case Some("icmp") => {
          for {
            host <- asString(mn \ "host")
            ipv6 = asString(mn \ "ipv6").map(_.toBoolean).getOrElse(false)
          } yield {
            ICMPFunctionalCheck(host, ipv6)
          }
        }
        case Some("jmx") => {
          for {
            url <- asString(mn \ "url")
          } yield {
            JmxFunctionalCheck(url)
          }
        }
        case Some("munin") => {
          for {
            host <- asString(mn \ "host")
            port <- asString(mn \ "port").map(_.toInt)
          } yield {
            optOnKey(
              mn,
              "thresholds",
              tsv => {
                asArrayOfObjs(tsv).map(ts => {
                  val tsName = asString(ts \ "name").getOrElse("unknown")
                  val tsType = asString(ts \ "type").getOrElse("counter")
                  MuninCategoryDefinition(tsName, MuninFieldType.parse(tsType))
                })
              }
            ).map(onlyFetch => {
                MuninFunctionalCheck(host, port, onlyFetch)
              })
              .getOrElse({
                MuninFunctionalCheck(host, port)
              })

          }
        }
        case Some("latestDataExtractor") => {
          for (key <- asString(mn \ "key");
               dataAttributeName <- asString(mn \ "dataAttributeName")) yield {
            LatestDataExtractor(key, dataAttributeName)
          }
        }
        case Some("lastDataExtractor") => {
          for (key <- asString(mn \ "key");
               dataAttributeName <- asString(mn \ "dataAttributeName")) yield {
            LastDataExtractor(key, dataAttributeName)
          }
        }
        case Some("jmxExtractOsLoad") => {
          for (key <- asString(mn \ "key")) yield {
            JmxOsLoadExtractor(key)
          }
        }
        case Some("jmxExtractOsProcessorCount") => {
          for (key <- asString(mn \ "key")) yield {
            JmxOsProcessorCountExtractor(key)
          }
        }
        case Some("jmxExtractOsName") => {
          for (key <- asString(mn \ "key")) yield {
            JmxOsNameExtractor(key)
          }
        }
        case Some("jmxExtractOsArch") => {
          for (key <- asString(mn \ "key")) yield {
            JmxOsArchExtractor(key)
          }
        }
        case Some("jmxExtractOsVersion") => {
          for (key <- asString(mn \ "key")) yield {
            JmxOsVersionExtractor(key)
          }
        }
        case Some("jmxExtractRuntimeUptime") => {
          for (key <- asString(mn \ "key")) yield {
            JmxRuntimeUptimeExtractor(key)
          }
        }
        case Some("jmxExtractRuntimeInputArgs") => {
          for (key <- asString(mn \ "key")) yield {
            JmxRuntimeInputArgsExtractor(key)
          }
        }
        case Some("jmxExtractRuntimeName") => {
          for (key <- asString(mn \ "key")) yield {
            JmxRuntimeNameExtractor(key)
          }
        }
        case Some("jmxExtractHeapMemoryMax") => {
          for (key <- asString(mn \ "key")) yield {
            JmxHeapMemoryMaxExtractor(key)
          }
        }
        case Some("jmxExtractHeapMemoryUsed") => {
          for (key <- asString(mn \ "key")) yield {
            JmxHeapMemoryUsedExtractor(key)
          }
        }
        case Some("jmxExtractHeapMemoryCommitted") => {
          for (key <- asString(mn \ "key")) yield {
            JmxHeapMemoryCommittedExtractor(key)
          }
        }
        case Some("jmxExtractHeapMemoryInit") => {
          for (key <- asString(mn \ "key")) yield {
            JmxHeapMemoryInitExtractor(key)
          }
        }
        case Some("jmxExtractHeapMemoryPercentage") => {
          for (key <- asString(mn \ "key")) yield {
            JmxHeapMemoryPercentageExtractor(key)
          }
        }
        case Some("jmxExtractNonHeapMemoryMax") => {
          for (key <- asString(mn \ "key")) yield {
            JmxNonHeapMemoryMaxExtractor(key)
          }
        }
        case Some("jmxExtractNonHeapMemoryUsed") => {
          for (key <- asString(mn \ "key")) yield {
            JmxNonHeapMemoryUsedExtractor(key)
          }
        }
        case Some("jmxExtractNonHeapMemoryCommitted") => {
          for (key <- asString(mn \ "key")) yield {
            JmxNonHeapMemoryCommittedExtractor(key)
          }
        }
        case Some("jmxExtractNonHeapMemoryInit") => {
          for (key <- asString(mn \ "key")) yield {
            JmxNonHeapMemoryInitExtractor(key)
          }
        }
        case Some("jmxExtractNonHeapMemoryPercentage") => {
          for (key <- asString(mn \ "key")) yield {
            JmxNonHeapMemoryPercentageExtractor(key)
          }
        }
        case Some("httpAttachBasicAuth") => {
          for (domain <- asString(mn \ "domain");
               username <- asString(mn \ "username");
               password <- asString(mn \ "password"))
            yield HttpAddBasicAuthorization(domain, username, password)
        }
        case Some("httpExtractRequestUrl") => {
          for (key <- asString(mn \ "key")) yield HttpRequestUrlExtractor(key)
        }
        case Some("httpExtractStatusCode") => {
          for (key <- asString(mn \ "key")) yield HttpStatusCodeExtractor(key)
        }
        case Some("httpExtractHeaderValue") => {
          for (key <- asString(mn \ "key");
               header <- asString(mn \ "header"))
            yield HttpHeaderExtractor(key, header)
        }
        case Some("httpExtractRedirectCount") => {
          for (key <- asString(mn \ "key"))
            yield HttpRedirectCountExtractor(key)
        }
        case Some("httpExtractRetryCount") => {
          for (key <- asString(mn \ "key")) yield HttpRetryCountExtractor(key)
        }
        case Some("httpExtractStartTime") => {
          for (key <- asString(mn \ "key")) yield HttpStartTimeExtractor(key)
        }
        case Some("httpExtractEndTime") => {
          for (key <- asString(mn \ "key")) yield HttpEndTimeExtractor(key)
        }
        case Some("httpExtractExceptions") => {
          for (key <- asString(mn \ "key")) yield HttpExceptionsExtractor(key)
        }
        case Some("storeSqlResultSet") => {
          for (key <- asString(mn \ "key")) yield StoreSqlResultSet(key)
        }
        case Some("sqlExtractRow") => {
          for (key <- asString(mn \ "key");
               row <- asString(mn \ "row").map(_.toInt);
               separator = asString(mn \ "separator"))
            yield SqlRowExtractor(key, row)
        }
        case Some("sqlExtractColumn") => {
          for (key <- asString(mn \ "key");
               col <- asString(mn \ "column");
               separator = asString(mn \ "separator"))
            yield SqlColExtractor(key, col, separator)
        }
        case Some("sqlExtractCell") => {
          for (key <- asString(mn \ "key");
               row <- asString(mn \ "row").map(_.toInt);
               col <- asString(mn \ "column")) yield {
            SqlCellExtractor(key, row, col)
          }
        }
        case Some("storeLdapResult") => {
          for (key <- asString(mn \ "key")) yield StoreLdapResults(key)
        }
        case Some("ldapExtractQuery") => {
          for (key <- asString(mn \ "key")) yield LdapQueryExtractor(key)
        }
        case Some("ldapExtractSearchBase") => {
          for (key <- asString(mn \ "key")) yield LdapSearchBaseExtractor(key)
        }
        case Some("ldapExtractRecord") => {
          for (key <- asString(mn \ "key");
               recordName <- asString(mn \ "recordName"))
            yield LdapRecordExtractor(key, recordName)
        }
        case Some("ldapExtractAttr") => {
          for (key <- asString(mn \ "key");
               attrName <- asString(mn \ "attrName"))
            yield LdapAttrExtractor(key, attrName)
        }
        case Some("ldapExtractAttrFromRecord") => {
          for (key <- asString(mn \ "key");
               recordName <- asString(mn \ "recordName");
               attrName <- asString(mn \ "attrName"))
            yield LdapAttrFromRecordExtractor(key, recordName, attrName)
        }
        case Some("setKey") => {
          for (key <- asString(mn \ "key");
               value <- asString(mn \ "value")) yield {
            KeySetter(key, value)
          }
        }
        case Some("deleteKey") => {
          for (key <- asString(mn \ "key")) yield {
            KeyDeleter(key)
          }
        }
        case Some("storeResult") => {
          for (key <- asString(mn \ "key")) yield {
            ResultStorer(key)
          }
        }
        case Some("storeMetaData") => {
          for (key <- asString(mn \ "key");
               metaDataKey <- asString(mn \ "metaDataKey")) yield {
            MetaDataStorer(key, metaDataKey)
          }
        }
        case Some("storeStatusCode") => {
          for (key <- asString(mn \ "key")) yield {
            StatusCodeStorer(key)
          }
        }
        case Some("xPathResult") => {
          for (key <- asString(mn \ "key");
               xPath <- asString(mn \ "xPath")) yield {
            XPathFromResult(key, xPath)
          }
        }
        case Some("foreachXPathResult") => {
          for (key <- asString(mn \ "key");
               xPath <- asString(mn \ "xPath");
               funcs = asArrayOfObjs(mn \ "do").flatMap(tn =>
                 configureFromJson(tn))) yield {
            ForeachXPathFromResult(key, xPath, funcs)
          }
        }
        case Some("regexResult") => {
          for (key <- asString(mn \ "key");
               regex <- asString(mn \ "regex")) yield {
            RegexFromResult(key, regex)
          }
        }
        case Some("foreachRegexResult") => {
          for (key <- asString(mn \ "key");
               regex <- asString(mn \ "regex");
               funcs = asArrayOfObjs(mn \ "do").flatMap(tn =>
                 configureFromJson(tn))) yield {
            ForeachRegexFromResult(key, regex, funcs)
          }
        }
        case Some("setResult") => {
          for (result <- asString(mn \ "result")) yield {
            ResultSetter(result)
          }
        }
        case Some("setResultToAttribute") => {
          for (key <- asString(mn \ "key")) yield {
            RetrieveAttributeToResult(key)
          }
        }
        case Some("if") => {
          for (key <- asString(mn \ "key");
               value <- asString(mn \ "value");
               thenFuncs = asArrayOfObjs(mn \ "then").flatMap(tn =>
                 configureFromJson(tn));
               elseFuncs = asArrayOfObjs(mn \ "else").flatMap(tn =>
                 configureFromJson(tn))) yield {
            Cond(key, value, thenFuncs, elseFuncs)
          }
        }
        case Some("while") => {
          for (key <- asString(mn \ "key");
               value <- asString(mn \ "value");
               funcs = asArrayOfObjs(mn \ "do").flatMap(tn =>
                 configureFromJson(tn))) yield {
            WhileLoop(key, value, funcs)
          }
        }
        case Some("for") => {
          for (key <- asString(mn \ "key");
               start <- asString(mn \ "start").map(_.toInt);
               end <- asString(mn \ "end").map(_.toInt);
               incrementing = (mn \ "incrementing")
                 .extractOpt[Boolean]
                 .getOrElse(true);
               funcs = asArrayOfObjs(mn \ "do").flatMap(tn =>
                 configureFromJson(tn))) yield {
            ForLoop(key, start, end, incrementing, funcs)
          }
        }
        case Some("delay") => {
          for (delay <- (mn \ "delay").extractOpt[Long];
               random = (mn \ "random").extractOpt[Boolean].getOrElse(false))
            yield {
              Delay(delay, random)
            }
        }
        case Some("stringEquals") => {
          for (key <- asString(mn \ "key");
               value <- asString(mn \ "value")) yield {
            EnvironmentValidator("%s equals %s".format(key, value),
                                 (env) => env.get(key).exists(_ == value))
          }
        }
        case Some("stringEqualsInsensitive") => {
          for (key <- asString(mn \ "key");
               value <- asString(mn \ "value").map(_.toLowerCase.trim)) yield {
            EnvironmentValidator(
              "%s equals %s".format(key, value),
              (env) => env.get(key).exists(_.toLowerCase.trim == value))
          }
        }
        case Some("numericallyLessThan") => {
          for (key <- asString(mn \ "key");
               value <- asString(mn \ "value").map(_.toDouble)) yield {
            EnvironmentValidator(
              "%s numerically less than %s".format(key, value),
              (env) => env.get(key).exists(_.toDouble < value))
          }
        }
        case Some("numericallyGreaterThan") => {
          for (key <- asString(mn \ "key");
               value <- asString(mn \ "value").map(_.toDouble)) yield {
            EnvironmentValidator(
              "%s numerically less than %s".format(key, value),
              (env) => env.get(key).exists(_.toDouble > value))
          }
        }
        case Some("numericallyEqualTo") => {
          for (key <- asString(mn \ "key");
               value <- asString(mn \ "value").map(_.toDouble)) yield {
            EnvironmentValidator(
              "%s numerically less than %s".format(key, value),
              (env) => env.get(key).exists(_.toDouble == value))
          }
        }
        case _ => None
      }
    })
  }

  def configureFromXml(n: Node): List[FunctionalServiceCheck] = {
    getImmediateNodes(n, "step").flatMap(mn => {
      getAttr(mn, "type").getOrElse("unknown") match {
        case "http" => {
          for (url <- getAttr(mn, "url");
               method <- getAttr(mn, "method");
               params = getNodes(mn, "parameter").flatMap(pn => {
                 for (key <- getAttr(pn, "key");
                      value <- getAttr(pn, "value")) yield {
                   (key, value)
                 }
               });
               headers = Map(getNodes(mn, "header").flatMap(hn => {
                 for (key <- getAttr(hn, "key");
                      value <- getAttr(hn, "value")) yield {
                   (key, value)
                 }
               }): _*);
               matcher: HTTPResponseMatcher = HTTPResponseMatchers
                 .configureFromXml(
                   <thresholdsPacket>{getNodes(mn,"thresholds")}</thresholdsPacket>))
            yield HttpFunctionalCheck(method, url, params, headers, matcher)
        }
        case "sql" => {
          for (driver <- getAttr(mn, "driver");
               url <- getAttr(mn, "url");
               username <- getAttr(mn, "username");
               password <- getAttr(mn, "password");
               query <- getAttr(mn, "query");
               connectionTimeout = getAttr(mn, "connectionTimeout").map(
                 _.toLong);
               matchers: List[VerifiableSqlResultSetDefinition] = getNodes(
                 mn,
                 "thresholds").map(ts => {
                 val rowBehaviour = getAttr(ts, "rows").getOrElse("all")
                 val tsMap = Matchers.configureFromXml(ts)
                 VerifiableSqlResultSetDefinition(rowBehaviour, tsMap)
               }))
            yield
              JDBCFunctionalCheck(driver,
                                  url,
                                  username,
                                  password,
                                  query,
                                  matchers,
                                  connectionTimeout.getOrElse(10000L))
        }
        case "ldap" => {
          for (host <- getAttr(mn, "url");
               username <- getAttr(mn, "username");
               password <- getAttr(mn, "password");
               searchBase <- getAttr(mn, "searchBase");
               query <- getAttr(mn, "query"))
            yield
              LdapFunctionalCheck(host, username, password, searchBase, query)
        }
        case "icmp" => {
          for (host <- getAttr(mn, "host");
               ipv6 = getAttr(mn, "ipv6").map(_.toBoolean).getOrElse(false))
            yield ICMPFunctionalCheck(host, ipv6)
        }
        case "jmx" => {
          for (url <- getAttr(mn, "url")) yield JmxFunctionalCheck(url)
        }
        case "munin" => {
          for (host <- getAttr(mn, "host");
               port <- getAttr(mn, "port").map(_.toInt)) yield {
            (mn \\ "thresholds").headOption
              .map(_ho =>
                getNodes(mn, "thresholds").map(ts => {
                  val tsName = getAttr(ts, "name").getOrElse("unknown")
                  val tsType = getAttr(ts, "type").getOrElse("counter")
                  MuninCategoryDefinition(tsName, MuninFieldType.parse(tsType))
                }))
              .map(onlyFetch => {
                MuninFunctionalCheck(host, port, onlyFetch)
              })
              .getOrElse({
                MuninFunctionalCheck(host, port)
              })
          }
        }
        case "latestDataExtractor" => {
          for (key <- getAttr(mn, "key");
               dataAttributeName <- getAttr(mn, "dataAttributeName")) yield {
            LatestDataExtractor(key, dataAttributeName)
          }
        }
        case "lastDataExtractor" => {
          for (key <- getAttr(mn, "key");
               dataAttributeName <- getAttr(mn, "dataAttributeName")) yield {
            LastDataExtractor(key, dataAttributeName)
          }
        }
        case "jmxExtractOsLoad" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxOsLoadExtractor(key)
          }
        }
        case "jmxExtractOsProcessorCount" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxOsProcessorCountExtractor(key)
          }
        }
        case "jmxExtractOsName" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxOsNameExtractor(key)
          }
        }
        case "jmxExtractOsArch" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxOsArchExtractor(key)
          }
        }
        case "jmxExtractOsVersion" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxOsVersionExtractor(key)
          }
        }
        case "jmxExtractRuntimeUptime" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxRuntimeUptimeExtractor(key)
          }
        }
        case "jmxExtractRuntimeInputArgs" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxRuntimeInputArgsExtractor(key)
          }
        }
        case "jmxExtractRuntimeName" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxRuntimeNameExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryMax" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxHeapMemoryMaxExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryUsed" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxHeapMemoryUsedExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryCommitted" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxHeapMemoryCommittedExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryInit" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxHeapMemoryInitExtractor(key)
          }
        }
        case "jmxExtractHeapMemoryPercentage" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxHeapMemoryPercentageExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryMax" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxNonHeapMemoryMaxExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryUsed" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxNonHeapMemoryUsedExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryCommitted" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxNonHeapMemoryCommittedExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryInit" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxNonHeapMemoryInitExtractor(key)
          }
        }
        case "jmxExtractNonHeapMemoryPercentage" => {
          for (key <- getAttr(mn, "key")) yield {
            JmxNonHeapMemoryPercentageExtractor(key)
          }
        }
        case "httpAttachBasicAuth" => {
          for (domain <- getAttr(mn, "domain");
               username <- getAttr(mn, "username");
               password <- getAttr(mn, "password"))
            yield HttpAddBasicAuthorization(domain, username, password)
        }
        case "httpExtractRequestUrl" => {
          for (key <- getAttr(mn, "key")) yield HttpRequestUrlExtractor(key)
        }
        case "httpExtractStatusCode" => {
          for (key <- getAttr(mn, "key")) yield HttpStatusCodeExtractor(key)
        }
        case "httpExtractHeaderValue" => {
          for (key <- getAttr(mn, "key");
               header <- getAttr(mn, "header"))
            yield HttpHeaderExtractor(key, header)
        }
        case "httpExtractRedirectCount" => {
          for (key <- getAttr(mn, "key")) yield HttpRedirectCountExtractor(key)
        }
        case "httpExtractRetryCount" => {
          for (key <- getAttr(mn, "key")) yield HttpRetryCountExtractor(key)
        }
        case "httpExtractStartTime" => {
          for (key <- getAttr(mn, "key")) yield HttpStartTimeExtractor(key)
        }
        case "httpExtractEndTime" => {
          for (key <- getAttr(mn, "key")) yield HttpEndTimeExtractor(key)
        }
        case "httpExtractExceptions" => {
          for (key <- getAttr(mn, "key")) yield HttpExceptionsExtractor(key)
        }
        case "storeSqlResultSet" => {
          for (key <- getAttr(mn, "key")) yield StoreSqlResultSet(key)
        }
        case "sqlExtractRow" => {
          for (key <- getAttr(mn, "key");
               row <- getAttr(mn, "row").map(_.toInt);
               separator = getAttr(mn, "separator"))
            yield SqlRowExtractor(key, row)
        }
        case "sqlExtractColumn" => {
          for (key <- getAttr(mn, "key");
               col <- getAttr(mn, "column");
               separator = getAttr(mn, "separator"))
            yield SqlColExtractor(key, col, separator)
        }
        case "sqlExtractCell" => {
          for (key <- getAttr(mn, "key");
               row <- getAttr(mn, "row").map(_.toInt);
               col <- getAttr(mn, "column")) yield {
            SqlCellExtractor(key, row, col)
          }
        }
        case "storeLdapResult" => {
          for (key <- getAttr(mn, "key")) yield StoreLdapResults(key)
        }
        case "ldapExtractQuery" => {
          for (key <- getAttr(mn, "key")) yield LdapQueryExtractor(key)
        }
        case "ldapExtractSearchBase" => {
          for (key <- getAttr(mn, "key")) yield LdapSearchBaseExtractor(key)
        }
        case "ldapExtractRecord" => {
          for (key <- getAttr(mn, "key");
               recordName <- getAttr(mn, "recordName"))
            yield LdapRecordExtractor(key, recordName)
        }
        case "ldapExtractAttr" => {
          for (key <- getAttr(mn, "key");
               attrName <- getAttr(mn, "attrName"))
            yield LdapAttrExtractor(key, attrName)
        }
        case "ldapExtractAttrFromRecord" => {
          for (key <- getAttr(mn, "key");
               recordName <- getAttr(mn, "recordName");
               attrName <- getAttr(mn, "attrName"))
            yield LdapAttrFromRecordExtractor(key, recordName, attrName)
        }
        case "setKey" => {
          for (key <- getAttr(mn, "key");
               value <- getAttr(mn, "value")) yield {
            KeySetter(key, value)
          }
        }
        case "deleteKey" => {
          for (key <- getAttr(mn, "key")) yield {
            KeyDeleter(key)
          }
        }
        case "storeResult" => {
          for (key <- getAttr(mn, "key")) yield {
            ResultStorer(key)
          }
        }
        case "storeMetaData" => {
          for (key <- getAttr(mn, "key");
               metaDataKey <- getAttr(mn, "metaDataKey")) yield {
            MetaDataStorer(key, metaDataKey)
          }
        }
        case "storeStatusCode" => {
          for (key <- getAttr(mn, "key")) yield {
            StatusCodeStorer(key)
          }
        }
        case "xPathResult" => {
          for (key <- getAttr(mn, "key");
               xPath <- getAttr(mn, "xPath")) yield {
            XPathFromResult(key, xPath)
          }
        }
        case "foreachXPathResult" => {
          for (key <- getAttr(mn, "key");
               xPath <- getAttr(mn, "xPath");
               funcs = getImmediateNodes(mn, "do").flatMap(tn =>
                 configureFromXml(tn))) yield {
            ForeachXPathFromResult(key, xPath, funcs)
          }
        }
        case "regexResult" => {
          for (key <- getAttr(mn, "key");
               regex <- getAttr(mn, "regex")) yield {
            RegexFromResult(key, regex)
          }
        }
        case "foreachRegexResult" => {
          for (key <- getAttr(mn, "key");
               regex <- getAttr(mn, "regex");
               funcs = getImmediateNodes(mn, "do").flatMap(tn =>
                 configureFromXml(tn))) yield {
            ForeachRegexFromResult(key, regex, funcs)
          }
        }
        case "setResult" => {
          for (result <- getAttr(mn, "result")) yield {
            ResultSetter(result)
          }
        }
        case "setResultToAttribute" => {
          for (key <- getAttr(mn, "key")) yield {
            RetrieveAttributeToResult(key)
          }
        }
        case "if" => {
          for (key <- getAttr(mn, "key");
               value <- getAttr(mn, "value");
               thenFuncs = getImmediateNodes(mn, "then").flatMap(tn =>
                 configureFromXml(tn));
               elseFuncs = getImmediateNodes(mn, "else").flatMap(tn =>
                 configureFromXml(tn))) yield {
            Cond(key, value, thenFuncs, elseFuncs)
          }
        }
        case "while" => {
          for (key <- getAttr(mn, "key");
               value <- getAttr(mn, "value");
               funcs = getImmediateNodes(mn, "do").flatMap(tn =>
                 configureFromXml(tn))) yield {
            WhileLoop(key, value, funcs)
          }
        }
        case "for" => {
          for (key <- getAttr(mn, "key");
               start <- getAttr(mn, "start").map(_.toInt);
               end <- getAttr(mn, "end").map(_.toInt);
               incrementing = getAttr(mn, "incrementing")
                 .map(_.trim.toLowerCase == "true")
                 .getOrElse(true);
               funcs = getImmediateNodes(mn, "do").flatMap(tn =>
                 configureFromXml(tn))) yield {
            ForLoop(key, start, end, incrementing, funcs)
          }
        }
        case "delay" => {
          for (delay <- getAttr(mn, "delay").map(_.toLong);
               random = getAttr(mn, "random")
                 .map(_.trim.toLowerCase == "true")
                 .getOrElse(false)) yield {
            Delay(delay, random)
          }
        }
        case "stringEquals" => {
          for (key <- getAttr(mn, "key");
               value <- getAttr(mn, "value")) yield {
            EnvironmentValidator("%s equals %s".format(key, value),
                                 (env) => env.get(key).exists(_ == value))
          }
        }
        case "stringEqualsInsensitive" => {
          for (key <- getAttr(mn, "key");
               value <- getAttr(mn, "value").map(_.toLowerCase.trim)) yield {
            EnvironmentValidator(
              "%s equals %s".format(key, value),
              (env) => env.get(key).exists(_.toLowerCase.trim == value))
          }
        }
        case "numericallyLessThan" => {
          for (key <- getAttr(mn, "key");
               value <- getAttr(mn, "value").map(_.toDouble)) yield {
            EnvironmentValidator(
              "%s numerically less than %s".format(key, value),
              (env) => env.get(key).exists(_.toDouble < value))
          }
        }
        case "numericallyGreaterThan" => {
          for (key <- getAttr(mn, "key");
               value <- getAttr(mn, "value").map(_.toDouble)) yield {
            EnvironmentValidator(
              "%s numerically less than %s".format(key, value),
              (env) => env.get(key).exists(_.toDouble > value))
          }
        }
        case "numericallyEqualTo" => {
          for (key <- getAttr(mn, "key");
               value <- getAttr(mn, "value").map(_.toDouble)) yield {
            EnvironmentValidator(
              "%s numerically less than %s".format(key, value),
              (env) => env.get(key).exists(_.toDouble == value))
          }
        }
        case _ => None
      }
    })
  }
}

abstract class FunctionalServiceCheck extends Logger {
  protected var see: Option[ScriptExecutionEnvironment] = None
  def attachScriptExecutionEnvironment(newSee: ScriptExecutionEnvironment) = {
    see = Some(newSee)
  }
  protected def innerAct(previousResult: FunctionalCheckReturn,
                         interpolator: Interpolator): FunctionalCheckReturn
  def act(
      previousResult: FunctionalCheckReturn,
      interpolator: Interpolator): Either[Exception, FunctionalCheckReturn] =
    try {
      debug(
        "STEP: %s \r\n (env: %s)".format(this,
                                         previousResult.updatedEnvironment))
      Right(innerAct(previousResult, interpolator))
    } catch {
      case e: Exception => Left(e)
    }
}
