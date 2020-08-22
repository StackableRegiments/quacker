package bootstrap.liftweb

import _root_.metl.model._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.util._
import Helpers._
import metl.view.{DebugToolsRestHelper, ProbeRestHelper, SystemRestHelper}

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify Lift's environment.
  */
class Boot extends Logger {
  implicit val formats = GraphableData.formats
  def boot {
    sys.props.put("h2.implicitRelativePath", "true")
    Globals.isDevMode = Props.mode match {
      case Props.RunModes.Production => false
      case _                         => true
    }
    LiftRules.attachResourceId = {
      if (Globals.isDevMode) { s =>
        "%s?%s".format(s, nextFuncName)
      } else {
        val prodRunId = nextFuncName
        s =>
          "%s?%s".format(s, prodRunId)
      }
    }
    val configurationStatus = ServiceConfigurator.describeAutoConfigure(
      ServiceConfigurator.autoConfigure)
    warn("Xml configuration reloaded\r\n%s".format(configurationStatus))

    // Setup RESTful endpoints (these are in view/Endpoints.scala)
    LiftRules.dispatch.prepend(ProbeRestHelper)
    // attempting to setup authentication using CAS, when running in prod mode or staging mode
    if (Globals.isDevMode) {
      //attach the debug-tools which break things
      LiftRules.dispatch.append(DebugToolsRestHelper)
    }
    LiftRules.dispatch.prepend(SystemRestHelper)

    // where to search snippet
    LiftRules.addToPackages("metl")

    // Build SiteMap
    def sitemap() =
      SiteMap(
        Menu("Home") / "index" >> User.AddUserMenusAfter, // Simple menu form
        Menu("Flexible") / "flexible",
        // Menu with special Link
        Menu(
          Loc("Static",
              Link(List("static"), true, "/static/index"),
              "Static Content"))
      )

    LiftRules.setSiteMapFunc(() => User.sitemapMutator(sitemap()))

    // added temporarily (for Lift 3.4.0, where the resource server appears to be aiming for non-existent *-min.js files)
    ResourceServer.pathRewriter = {
      case anything => anything
    }

    LiftRules.securityRules = () => SecurityRules(content = None)

    // Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart = Full(
      () => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd = Full(
      () => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    metl.model.Globals

    metl.comet.DashboardServer
  }

  /**
    * Force the request to be UTF-8
    */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
