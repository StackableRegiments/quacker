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

import net.liftweb.http.js.JE.Call

class Header extends CometActor with CometListener {
  override def lifespan:Box[TimeSpan] = Full(5 minutes)
  def registerWith: DashboardServer.type = DashboardServer
  override def render: RenderOut = {
    OnLoad(Call("setDevMode",JBool(Globals.isDevMode)))
  }
}