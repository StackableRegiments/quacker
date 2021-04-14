package com.metl.liftAuthenticator

import net.liftweb.http._
import net.liftweb.common._

case class LiftAuthStateData(authenticated: Boolean,
                             username: String,
                             eligibleGroups: Seq[(String, String)],
                             informationGroups: Seq[(String, String)])
object LiftAuthStateDataForbidden
    extends LiftAuthStateData(false,
                              "forbidden",
                              List.empty[Tuple2[String, String]],
                              List.empty[Tuple2[String, String]]) {}

object LiftAuthAuthentication {
  def attachAuthenticator(mod: LiftAuthenticationSystem): Unit = {
    LiftRules.dispatch.prepend {
      case req if mod.dispatchTableItemFilter(req) =>
        () =>
          mod.dispatchTableItem(req)
    }
  }
}

trait LiftAuthenticationSystem {
  def dispatchTableItemFilter: Req => Boolean
  def dispatchTableItem(req: Req): Box[LiftResponse]
}

abstract class LiftAuthenticator(alreadyLoggedIn: () => Boolean,
                                 onSuccess: (LiftAuthStateData) => Unit) {
  object InSessionLiftAuthState
      extends SessionVar[LiftAuthStateData](LiftAuthStateDataForbidden)
  def checkWhetherAlreadyLoggedIn: Boolean =
    alreadyLoggedIn() || InSessionLiftAuthState.is.authenticated
  def constructResponse(input: Req): LiftResponse
}
