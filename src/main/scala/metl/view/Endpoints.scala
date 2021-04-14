package metl.view

import metl.model.{HistoryServer, Servers, ServiceConfigurator}
import net.liftweb.common.{Full, Logger}
import net.liftweb.http._
import net.liftweb.http.rest.RestHelper
import net.liftweb.util.Helpers.tryo
import metl.model.Globals
import com.metl.liftAuthenticator._

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
    case r @ Req("logout" :: _, _, _) => {
      Globals.setUser(LiftAuthStateDataForbidden)
      Full(RedirectResponse(r.param("returnTo").getOrElse("/")))
    }
  }
}

object ProbeRestHelper extends RestHelper with Logger {
  val probeEndpoints = List("probe", "healthz", "serverStatus")
  serve {
    case Req(cand :: _, _, _) if probeEndpoints.contains(cand) =>
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

trait Authenticator {
  def loginLink: String
  def attach: Unit
}

class MockAuthenticator extends Authenticator with RestHelper with Logger {
  def loginLink = "/login/mock"
  def attach = {
    LiftRules.dispatch.prepend(this)
  }
  serve {
    case r @ Req("login" :: "mock" :: _, _, _) =>
      () =>
        Full({
          val returnTo =
            r.param("returnTo").filter(_.trim() != "").getOrElse("/")
          (for {
            username <- r.param("username")
          } yield {
            val userState =
              LiftAuthStateData(true, username, Nil, Nil)
            Globals.setUser(userState)
            RedirectResponse(returnTo)
          }).getOrElse({
            val mockLoginPage =
              <div>
  <form method="GET" action="/login/mock">
    <input type="hidden" name="returnTo" value={returnTo}></input>
    <input id="username" type="text" name="username"></input>
    <input type="submit" value="login">login</input>
  </form>
  <script>{
    """document.getElementById("username").focus();
    document.getElementById("username").onkeyup = function(evt){
      if (evt.keycode == 13){
        document.forms[0].submit();
      }
    }"""
  }</script>
</div>
            XhtmlResponse(mockLoginPage, None, Nil, Nil, 200, true)
          })
        })
  }
}

class GithubAuthenticator(
    clientId: String,
    clientSecret: String,
    redirect_host: String,
    scopes: List[String] = List("read:user"),
    githubAuthorizeEndpoint: String = "https://github.com/login/oauth/authorize",
    githubCodeExchangeEndpoint: String =
      "https://github.com/login/oauth/access_token",
    githubApiEndpoint: String = "https://api.github.com"
) extends RestHelper
    with Logger
    with Authenticator {
  import com.metl.utils.{HTTPResponse, Http}
  import net.liftweb.json._
  import Serialization._
  import net.liftweb.util.Helpers._
  def loginLink = "/redirect/github"
  def attach = {
    LiftRules.dispatch.prepend(this)
  }
  object State extends SessionVar[Option[String]](None)
  val redirect_uri = redirect_host + "/login/github"
  val stateSeparator = ":::"
  serve {
    case r @ Req("login" :: "github" :: _, _, _) =>
      () =>
        {
          for {
            code <- r.param("code")
            s <- State
          } yield {
            val client = Http.getClient
            client.addHttpHeader("Accept", "application/xml")
            val postBody = List(
              "client_id" -> clientId,
              "client_secret" -> clientSecret,
              "code" -> code,
              "redirect_uri" -> redirect_uri,
              "state" -> s
            )
            trace("sending request to github: %s".format(postBody))
            val postResponse = client.respondToResponse(
              client.postFormExpectingHTTPResponse(githubCodeExchangeEndpoint,
                                                   postBody,
                                                   Nil))
            val postResponseRaw = postResponse.responseAsString
            trace(
              "received response from github: %s\r\n%s".format(postResponse,
                                                               postResponseRaw))
            val xml = scala.xml.XML.loadString(postResponseRaw)
            val token = (xml \\ "access_token").headOption.map(_.text).get
            val userRecordUrl = githubApiEndpoint + "/user"
            val client2 = Http.getClient
            client2.addHttpHeader("Accept", "application/json")
            client2.addHttpHeader("User-Agent",
                                  "quacker @ %s".format(Globals.hostname))
            client2.addHttpHeader("Authorization", "token %s".format(token))
            val userRecordResponse = client2.respondToResponse(
              client2.getExpectingHTTPResponse(githubApiEndpoint + "/user"))
            val jsonRaw = userRecordResponse.responseAsString
            trace(
              "received user record: %s\r\n%s".format(userRecordResponse,
                                                      jsonRaw))
            val json = parse(jsonRaw)
            val username = (json \ "login").extractOpt[String].get
            State(None)
            val returnTo = s.split(stateSeparator).toList match {
              case List(key, path) => path
              case _               => "/"
            }
            trace(
              "logged in as: %s, returning to: %s".format(username, returnTo))
            val userState =
              LiftAuthStateData(true, username, Nil, Nil)
            Globals.setUser(userState)
            RedirectResponse(returnTo)
          }
        }
    case r @ Req("redirect" :: "github" :: _, _, _) =>
      () =>
        {
          val returnTo = r.param("returnTo").getOrElse("/")
          Globals.casState.is.authenticated match {
            case true => {
              Full(RedirectResponse(returnTo))
            }
            case false => {
              val s = "%s%s%s".format(nextFuncName, stateSeparator, returnTo)
              State(Some(s))
              Full(
                RedirectResponse(
                  "%s?client_id=%s&redirect_uri=%s&scope=%s&state=%s".format(
                    githubAuthorizeEndpoint,
                    urlEncode(clientId),
                    urlEncode(redirect_uri),
                    urlEncode(scopes.mkString(" ")),
                    urlEncode(s))))
            }
          }
        }
  }
}
