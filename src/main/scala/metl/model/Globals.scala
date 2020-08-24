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

import metl.view.{Authenticator, GithubAuthenticator, MockAuthenticator}

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
  protected var repo: Option[Repository] = None
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
  val hostname = (appConf \ "hostname").head.text
  println("starting up with hostname: %s".format(hostname))
  val authenticator: Option[Authenticator] =
    (appConf \ "authenticator").headOption.flatMap(authXml => {
      val authChildren = authXml.child.filter {
        case e: Elem => true
        case _       => false
      }.toList
      if (authChildren.length > 1) {
        throw new Exception(
          "too many authenticators configured - Quacker only supports one authentication context")
      }
      authChildren.headOption.flatMap(authenticatorElem => {
        authenticatorElem match {
          case ghc: Elem if ghc.label == "githubAuthenticator" => {
            for {
              ghClientId <- (ghc \ "@clientId").headOption.map(_.text)
              ghClientSecret <- (ghc \ "@clientSecret").headOption.map(_.text)
            } yield {
              new GithubAuthenticator(ghClientId, ghClientSecret, hostname)
            }
          }
          case mc: Elem if mc.label == "mockAuthenticator" =>
            Some(new MockAuthenticator)
          case _ => None
        }
      })
    })
  authenticator.foreach(_.attach)

  val superUsers: List[String] = ((appConf \ "superUsers") \ "superUser").toList
    .flatMap(n => (n \ "@username").map(_.text))
    .toList

  def startup = {
    metl.comet.DashboardServer
    repo = (appConf \ "repository").headOption.flatMap(repoXml => {
      val repoChildren = repoXml.child.filter {
        case e: Elem => true
        case _       => false
      }.toList
      if (repoChildren.length > 1) {
        throw new Exception(
          "too many repos configured - Quacker only supports one repo")
      }
      repoChildren match {
        case dbc: Elem if dbc.label == "sqlRepository" =>
          for {
            driver <- (dbc \ "@driver").headOption.map(_.text)
            url <- (dbc \ "@url").headOption.map(_.text)
            username = (dbc \ "@username").headOption.map(_.text).getOrElse("")
            password = (dbc \ "@password").headOption.map(_.text).getOrElse("")
          } yield {
            new SqlRepository(driver, url, username, password)
          }
        case mc: Elem if mc.label == "inMemoryRepository" =>
          Some(new InMemoryRepository)
        case _ => None
      }
    })

    // Setup RESTful endpoints (these are in view/Endpoints.scala)
    LiftRules.statelessDispatch.prepend(metl.view.ProbeRestHelper)
    LiftRules.dispatch.prepend(metl.view.SystemRestHelper)
  }

  var isDevMode = false

  def isSuperUser: Boolean = superUsers.contains(casState.is.username)
  def repository: Repository = repo.getOrElse(NullRepository)

  //Globals for the current session
  def setUser(newUser: LiftAuthStateData): Unit = {
    casState(newUser)
    currentUserAccessRestriction.update
    S.session.foreach(s =>
      metl.comet.DashboardServer ! metl.comet.UserLoggedIn(s))
  }
  object casState
      extends SessionVar[LiftAuthStateData](LiftAuthStateDataForbidden)
  object currentUserAccessRestriction {
    protected var store = updatedUserAccessRestriction
    def update = {
      store = updatedUserAccessRestriction
    }
    def permit(input: AnyRef): Boolean = doGet.permit(input)
    protected def doGet = store
    def get = doGet
    def is = doGet
  }

  def setValidUsers(newUsers: List[UserAccessRestriction]) = {
    newUsers.foreach(uar => {
      repository.updateValidUser(uar.id, Some(uar))
    })
  }
  protected def updatedUserAccessRestriction: UserAccessRestriction = {
    val me = Globals.casState.is.username
    val validUserAccessesForMe =
      repository.getValidUsers.filter(vua => vua.name == me)
    val myRestriction = validUserAccessesForMe.length match {
      case 0 =>
        UserAccessRestriction(me,
                              me,
                              List(
                                new ServicePermission("Public Access Only",
                                                      None,
                                                      None,
                                                      None,
                                                      None)))
      case other =>
        UserAccessRestriction(me,
                              me,
                              validUserAccessesForMe
                                .map(vua => vua.servicePermissions)
                                .flatten
                                .toList)
    }
    trace("myUserAccessRestriction: %s".format(myRestriction))
    myRestriction
  }
  def clearValidUsers = {
    repository.getValidUsers.foreach(vu => {
      repository.updateValidUser(vu.id, None)
    })
  }
  def isValidUser =
    repository.getValidUsers.exists(vua =>
      vua.name == Globals.casState.is.username)
}
