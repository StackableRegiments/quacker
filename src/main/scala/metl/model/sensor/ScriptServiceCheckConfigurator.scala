package metl.model.sensor

import metl.model.{ConfigFileReader, Matchers}
import net.liftweb.common.Logger

import scala.xml.Node

object FunctionalServiceCheck extends ConfigFileReader {
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
								timeout:Option[Long] = getAttr(mn, "timeout").headOption.map(_.toLong);
							 body = getNodes(mn, "body").headOption.map(_.text);
               headers = Map(getNodes(mn, "header").flatMap(hn => {
                 for (key <- getAttr(hn, "key");
                      value <- getAttr(hn, "value")) yield {
                   (key, value)
                 }
               }): _*);
               matcher: HTTPResponseMatcher = HTTPResponseMatchers
                 .configureFromXml(
                   <thresholdsPacket>{getNodes(mn,"thresholds")}</thresholdsPacket>))
            yield AsyncHttpFunctionalCheck(method, url, params, headers, body, matcher,timeout)
        }

        case "syncHttp" => {
          for (url <- getAttr(mn, "url");
               method <- getAttr(mn, "method");
               params = getNodes(mn, "parameter").flatMap(pn => {
                 for (key <- getAttr(pn, "key");
                      value <- getAttr(pn, "value")) yield {
                   (key, value)
                 }
               });
							 body = getNodes(mn, "body").headOption.map(_.text);
               headers = Map(getNodes(mn, "header").flatMap(hn => {
                 for (key <- getAttr(hn, "key");
                      value <- getAttr(hn, "value")) yield {
                   (key, value)
                 }
               }): _*);
               matcher: HTTPResponseMatcher = HTTPResponseMatchers
                 .configureFromXml(
                   <thresholdsPacket>{getNodes(mn,"thresholds")}</thresholdsPacket>))
            yield HttpFunctionalCheck(method, url, params, headers, body, matcher)
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
                         interpolator: Interpolator,
			callback: Either[Throwable, FunctionalCheckReturn] => Unit):Unit
  def act(
      previousResult: FunctionalCheckReturn,
      interpolator: Interpolator,
			callback: Either[Throwable, FunctionalCheckReturn] => Unit):Unit = {
      trace( "STEP: %s \r\n (env: %s)".format(this, previousResult.updatedEnvironment))
      innerAct(previousResult, interpolator,callback)
	}
}
