package metl.view

import metl.model.{HistoryServer, Servers, ServiceConfigurator}
import net.liftweb.common.{Full, Logger}
import net.liftweb.http._
import net.liftweb.http.rest.RestHelper
import net.liftweb.util.Helpers.tryo

object SystemRestHelper extends RestHelper with Logger {
  serve {
    case r @ Req(List("history", service, server, serviceCheckName), _, _) =>
      () =>
        {
          val checks = HistoryServer.getHistory(None,
                                                service,
                                                server,
                                                serviceCheckName,
                                                r.param("since").map(_.toLong),
                                                r.param("until").map(_.toLong),
                                                r.param("limit").map(_.toInt))
          Full(JsonResponse(net.liftweb.json.Extraction.decompose(checks), 200))
        }
    case r @ Req(List("history",
                      historyListenerName,
                      service,
                      server,
                      serviceCheckName),
                 _,
                 _) =>
      () =>
        {
          val checks = HistoryServer.getHistory(Some(historyListenerName),
                                                service,
                                                server,
                                                serviceCheckName,
                                                r.param("since").map(_.toLong),
                                                r.param("until").map(_.toLong),
                                                r.param("limit").map(_.toInt))
          Full(JsonResponse(net.liftweb.json.Extraction.decompose(checks), 200))
        }
    case r @ Req("allHistory" :: _, _, _) =>
      () =>
        {
          val checks =
            HistoryServer.getAllHistory(r.param("since").map(_.toLong),
                                        r.param("until").map(_.toLong),
                                        r.param("limit").map(_.toInt))
          Full(JsonResponse(net.liftweb.json.Extraction.decompose(checks), 200))
        }
    case Req("reloadXml" :: _, _, _) =>
      () =>
        {
          val configurationStatus = ServiceConfigurator.describeAutoConfigure(
            ServiceConfigurator.autoConfigure)
          Full(
            PlainTextResponse(
              "Xml configuration reloaded\r\n%s".format(configurationStatus),
              List.empty[Tuple2[String, String]],
              200))
        }
  }
}

object ProbeRestHelper extends RestHelper with Logger {
  serve {
    case Req("probe" :: _, _, _) =>
      () =>
        Full(PlainTextResponse("OK", List.empty[Tuple2[String, String]], 200))
    case Req("serverStatus" :: _, _, _) =>
      () =>
        Full(PlainTextResponse("OK", List.empty[Tuple2[String, String]], 200))
  }
}

object DebugToolsRestHelper extends RestHelper {
  serve {
    case Req("breakSomething" :: count :: _, _, _) =>
      () =>
        {
          val serversToBreak = tryo(count.toInt).openOr(3)
          Servers.breakSomething(serversToBreak)
          Full(
            PlainTextResponse("%s servers broken".format(serversToBreak),
                              List.empty[Tuple2[String, String]],
                              200))
        }
    case Req("breakSomething" :: _, _, _) =>
      () =>
        {
          val serversToBreak = 3
          Servers.breakSomething(serversToBreak)
          Full(
            PlainTextResponse("%s servers broken".format(serversToBreak),
                              List.empty[Tuple2[String, String]],
                              200))
        }
  }
}
