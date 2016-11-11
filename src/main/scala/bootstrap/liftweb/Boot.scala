package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.metl.model._
import com.metl.liftAuthenticator._
import com.metl.cas._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  implicit val formats = net.liftweb.json.DefaultFormats
  def boot {
		Globals.isDevMode = Props.mode match {
			case Props.RunModes.Production => false
			case _ => true
		}
		ServiceConfigurator.autoConfigure	
    //attach the probe handlers
    LiftRules.dispatch.prepend {
      case Req("probe" :: _,_,_) => () => Full(PlainTextResponse("OK",List.empty[Tuple2[String,String]], 200))
      case Req("serverStatus" :: _,_,_) => () => Full(PlainTextResponse("OK",List.empty[Tuple2[String,String]], 200))
    }

    // attempting to setup authentication using CAS, when running in prod mode or staging mode
    if (Globals.isDevMode) { 
			//attach the debug-tools which break things
			LiftRules.dispatch.append {
				case Req("breakSomething" :: count :: _,_,_) => () => {
					val serversToBreak = tryo(count.toInt).openOr(3)
					Servers.breakSomething(serversToBreak)
					Full(PlainTextResponse("%s servers broken".format(serversToBreak),List.empty[Tuple2[String,String]], 200))
				}
				case Req("breakSomething" :: _,_,_) => () => {
					val serversToBreak = 3
					Servers.breakSomething(serversToBreak)
					Full(PlainTextResponse("%s servers broken".format(serversToBreak),List.empty[Tuple2[String,String]], 200))
				}
			}
		}
		LiftRules.dispatch.append {
      case Req(List("history",service,server,label),_,_) => () => {
        val checks = HistoryServer.getHistory(None,service,server,label)
        Full(JsonResponse(net.liftweb.json.Extraction.decompose(checks),200))
      }
      case Req(List("history",historyListenerName,service,server,label),_,_) => () => {
        val checks = HistoryServer.getHistory(Some(historyListenerName),service,server,label)
        Full(JsonResponse(net.liftweb.json.Extraction.decompose(checks),200))
      }
			case Req("reloadXml" :: _,_,_) => () => {
				val configurationStatus = ServiceConfigurator.describeAutoConfigure(ServiceConfigurator.autoConfigure)
				Full(PlainTextResponse("Xml configuration reloaded\r\n%s".format(configurationStatus),List.empty[Tuple2[String,String]], 200))
			}
		}

    // where to search snippet
    LiftRules.addToPackages("metl")

    // Build SiteMap
    def sitemap() = SiteMap(
      Menu("Home") / "index" >> User.AddUserMenusAfter, // Simple menu form
			Menu("Dashboard") / "dashboard",
			Menu("FlexibleDashboard") / "flexibleDashboard",
      // Menu with special Link
      Menu(Loc("Static", Link(List("static"), true, "/static/index"), "Static Content")))
    LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

		metl.comet.DashboardServer
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
