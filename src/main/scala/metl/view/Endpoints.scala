package metl.view

import metl.model.{HistoryServer, Servers, ServiceConfigurator}
import net.liftweb.common.{Full, Logger}
import net.liftweb.http._
import net.liftweb.http.rest.RestHelper
import net.liftweb.util.Helpers.tryo

trait Stemmer {
  def stem(in: String): Tuple2[String, String] = {
    (("00000" + in).reverse.slice(3, 5).reverse, in)
  }
}

object SystemRestHelper extends RestHelper with Stemmer with Logger {
  serve {
    case Req(List("history", service, server, serviceCheckName), _, _) => () => {
      val checks = HistoryServer.getHistory(None, service, server, serviceCheckName)
      Full(JsonResponse(net.liftweb.json.Extraction.decompose(checks), 200))
    }
    case Req(List("history", historyListenerName, service, server, serviceCheckName), _, _) => () => {
      val checks = HistoryServer.getHistory(Some(historyListenerName), service, server, serviceCheckName)
      Full(JsonResponse(net.liftweb.json.Extraction.decompose(checks), 200))
    }
    case Req("allHistory" :: _, _, _) => () => {
      val checks = HistoryServer.getAllHistory
      Full(JsonResponse(net.liftweb.json.Extraction.decompose(checks), 200))
    }
    case Req("reloadXml" :: _, _, _) => () => {
      val configurationStatus = ServiceConfigurator.describeAutoConfigure(ServiceConfigurator.autoConfigure)
      Full(PlainTextResponse("Xml configuration reloaded\r\n%s".format(configurationStatus), List.empty[Tuple2[String, String]], 200))
    }
  }
}

object ProbeRestHelper extends RestHelper with Logger {
  serve {
    case Req("probe" :: _, _, _) => () => Full(PlainTextResponse("OK", List.empty[Tuple2[String, String]], 200))
    case Req("serverStatus" :: _, _, _) => () => Full(PlainTextResponse("OK", List.empty[Tuple2[String, String]], 200))
  }
}

object DebugToolsRestHelper extends RestHelper {
  serve {
    case Req("breakSomething" :: count :: _, _, _) => () => {
      val serversToBreak = tryo(count.toInt).openOr(3)
      Servers.breakSomething(serversToBreak)
      Full(PlainTextResponse("%s servers broken".format(serversToBreak), List.empty[Tuple2[String, String]], 200))
    }
    case Req("breakSomething" :: _, _, _) => () => {
      val serversToBreak = 3
      Servers.breakSomething(serversToBreak)
      Full(PlainTextResponse("%s servers broken".format(serversToBreak), List.empty[Tuple2[String, String]], 200))
    }
  }
}
