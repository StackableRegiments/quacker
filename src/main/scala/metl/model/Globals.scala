package metl.model

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import scala.xml._
import scala.collection.mutable.{HashMap, SynchronizedMap, ListBuffer}
import scala.collection.JavaConversions._
import com.mongodb.BasicDBObject
import com.metl.liftAuthenticator._
import com.metl.cas._

import scala.collection.JavaConverters._

object EnvVariable extends Logger {
  protected val environmentVariables: Map[String, String] =
    System.getenv.asScala.toMap;
  info(environmentVariables)
  protected def trimSystemProp(in: String): Box[String] = {
    try {
      var value = in.trim
      if (value.startsWith("\"")) {
        value = value.drop(1)
      }
      if (value.endsWith("\"")) {
        value = value.reverse.drop(1).reverse
      }
      Full(value)
    } catch {
      case e: Exception =>
        ParamFailure("exception while getting systemProperty",
                     Full(e),
                     Empty,
                     in)
    }
  }
  def getProp(systemEnvName: String, javaPropName: String): Box[String] = {
    environmentVariables
      .get(systemEnvName)
      .filterNot(v => v == null || v == "")
      .map(v => trimSystemProp(v))
      .getOrElse({
        val value = net.liftweb.util.Props
          .get(javaPropName)
          .map(v => Full(v))
          .openOr(Full(System.getProperty(javaPropName)))
        trace("getting from java prop: %s => %s".format(javaPropName, value))
        value
      })
  }
}

object Globals extends Logger {
  import java.io.File
  //Globals for the system
  println("starting up Globals")
  var configDirectoryLocation = "config"
  EnvVariable
    .getProp("QUACKER_CONFIG_DIRECTORY_LOCATION",
             "quacker.configDirectoryLocation")
    .map(qcdl => {
      trace("setting config directory location to: %s".format(qcdl))
      configDirectoryLocation = qcdl
      qcdl
    })
    .openOr({
      throw new Exception("no config directory location passed")
    })
  println("determined configDirectory: %s".format(configDirectoryLocation))
  var appConfigDirectoryLocation = "appConf"
  EnvVariable
    .getProp("QUACKER_APP_CONFIG_DIRECTORY_LOCATION",
             "quacker.appConfigDirectoryLocation")
    .map(qcdl => {
      trace("setting appConfig directory location to: %s".format(qcdl))
      appConfigDirectoryLocation = qcdl
      qcdl
    })
    .openOr({
      throw new Exception("no app config directory location passed")
    })
  println(
    "determined appConfigDirectory: %s".format(appConfigDirectoryLocation))
  val appConf =
    scala.xml.XML.loadFile(appConfigDirectoryLocation + "/application.xml")
  println("found xml: %s".format(appConf))
  val hostname = (appConf \ "hostname").head.text
  println("starting up with hostname: %s".format(hostname))
  for {
    ghc <- (appConf \ "githubAuthenticator").headOption
    ghClientId <- (ghc \ "@clientId").headOption.map(_.text)
    ghClientSecret <- (ghc \ "@clientSecret").headOption.map(_.text)
  } yield {
    println("creating GitHubAuthHelper from: %s".format(ghc))
    LiftRules.dispatch.prepend(
      new metl.view.GithubAuthHelper(ghClientId, ghClientSecret, hostname))
  }

  var isDevMode = false
  protected var validUserAccesses: List[UserAccessRestriction] =
    List.empty[UserAccessRestriction]
  def setValidUsers(newUsers: List[UserAccessRestriction]) = {
    validUserAccesses = (validUserAccesses ::: newUsers).toList
    currentUserAccessRestriction(updatedUserAccessRestriction)
  }
  protected def updatedUserAccessRestriction: UserAccessRestriction = {
    val me = currentUser.is
    val validUserAccessesForMe = validUserAccesses.filter(vua => vua.name == me)
    val myRestriction = validUserAccessesForMe.length match {
      case 0 =>
        UserAccessRestriction(me,
                              List(
                                new ServicePermission("Public Access Only",
                                                      None,
                                                      None,
                                                      None,
                                                      None)))
      case other =>
        UserAccessRestriction(me,
                              validUserAccessesForMe
                                .map(vua => vua.servicePermissions)
                                .flatten
                                .toList)
    }
    trace("myUserAccessRestriction: %s".format(myRestriction))
    myRestriction
  }
  def clearValidUsers = {
    validUserAccesses = List.empty[UserAccessRestriction]
  }
  def isValidUser = validUserAccesses.exists(vua => vua.name == currentUser.is)
  //Globals for the current session
  def setUser(newUser: LiftAuthStateData): Unit = {
    casState(newUser)
    currentUser(newUser.username)
    currentUserAccessRestriction(updatedUserAccessRestriction)
  }
  object casState
      extends SessionVar[LiftAuthStateData](LiftAuthStateDataForbidden)
  object currentUser extends SessionVar[String](casState.is.username)
  object currentUserAccessRestriction
      extends SessionVar[UserAccessRestriction](updatedUserAccessRestriction)
}
