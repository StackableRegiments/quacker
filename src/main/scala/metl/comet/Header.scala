package metl.comet

import metl.model._
import net.liftweb._
import net.liftweb.common._
import http._
import js._
import JsCmds._
import json.JsonAST._
import util._
import Helpers._
import scala.xml._

import net.liftweb.http.js.JE.Call

class Header extends CometActor with CometListener {
  override def lifespan: Box[TimeSpan] = Full(5 minutes)
  def registerWith: DashboardServer.type = DashboardServer
  override def lowPriority = {
    case UserLoggedIn(s) if S.session.exists(_ == s) => {
      reRender
    }
    case _ => {}
  }
  override def fixedRender = {
    "#loginLinkContainer" #> NodeSeq.Empty &
      "#headerScript *" #> OnLoad(Call("setDevMode", JBool(Globals.isDevMode)))
  }
  override def sendInitialReq_? = true

  protected var theReq: Box[Req] = Empty
  override def captureInitialReq(initialReq: Box[Req]): Unit = {
    theReq = initialReq
  }
  protected def reqPath: Option[String] =
    theReq.map(req => {
      req.request.uri
    })
  override def render: RenderOut = {
    val res: CssSel = Globals.authenticator
      .map(auth => {
        val withAuth: CssSel = {
          Globals.casState.is.authenticated match {
            case true => {
              "#loginLink [href]" #> "/logout?returnTo=%s".format(
                urlEncode(reqPath.getOrElse("/"))) &
                "#loginLinkName *" #> "Logout"
            }
            case false => {
              "#loginLink [href]" #> (auth.loginLink + "?returnTo=%s".format(
                urlEncode(reqPath.getOrElse("/")))) &
                "#loginLinkName *" #> "Login"
            }
          }
        } & "#loggedInUser *" #> Some(Globals.casState.is.username)
          .filter(_unused => Globals.casState.is.authenticated)
        withAuth
      })
      .getOrElse({
        val noAuth: CssSel = "#loginLinkContainer" #> NodeSeq.Empty
        noAuth
      })
    res & "#headerScript" #> NodeSeq.Empty
  }
}
