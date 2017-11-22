package metl.model

import com.metl.auth.OpenIdConnectAuthenticationSystem
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.util.Helpers._

import scala.xml._
import com.metl.liftAuthenticator._

import scala.collection.JavaConverters._
import scala.collection.immutable

object EnvVariable extends Logger {
  protected val environmentVariables:Map[String,String] = System.getenv.asScala.toMap;
  info(environmentVariables)
  protected def trimSystemProp(in:String):Box[String] = {
    try {
      var value = in.trim
      if (value.startsWith("\"")){
        value = value.drop(1)
      }
      if (value.endsWith("\"")){
        value = value.reverse.drop(1).reverse
      }
      Full(value)
    } catch {
      case e:Exception => ParamFailure("exception while getting systemProperty",Full(e),Empty,in)
    }
  }
  def getProp(systemEnvName:String,javaPropName:String):Box[String] = {
    environmentVariables.get(systemEnvName).filterNot(v => v == null || v == "").map(v => trimSystemProp(v)).getOrElse({
      val value = net.liftweb.util.Props.get(javaPropName).map(v => Full(v)).openOr(Full(System.getProperty(javaPropName)))
      trace("getting from java prop: %s => %s".format(javaPropName,value))
      value
    })
  }
}

object Globals extends Logger {
  //Globals for the system
  var configDirectoryLocation = "config"
  EnvVariable.getProp("QUACKER_CONFIG_DIRECTORY_LOCATION","quacker.configDirectoryLocation").map(qcdl => {
    trace("setting config directory location to: %s".format(qcdl))
    configDirectoryLocation = qcdl
    qcdl
  }).openOr({
    throw new Exception("no config directory location passed")
  })

  var configFile: NodeSeq = NodeSeq.Empty
  EnvVariable.getProp("QUACKER_CONFIG_FILE","quacker.configFile").map(qcf => {
    trace("setting config directory location to: %s".format(qcf))
    configFile = scala.xml.XML.loadFile(qcf)
    qcf
  }).openOr({
    throw new Exception("no config file location passed")
  })

  var isDevMode = false
	protected var validUserAccesses:List[UserAccessRestriction] = List.empty[UserAccessRestriction]
	def setValidUsers(newUsers:List[UserAccessRestriction]): UserAccessRestriction = {
		validUserAccesses = (validUserAccesses ::: newUsers).toList
		currentUserAccessRestriction(updatedUserAccessRestriction)
	}
	protected def updatedUserAccessRestriction:UserAccessRestriction = {
		val me = currentUser.is
		val validUserAccessesForMe = validUserAccesses.filter(vua => vua.name == me)
		val myRestriction = validUserAccessesForMe.length match {
			case 0 => UserAccessRestriction(me,List(new ServicePermission("Public Access Only",None,None,None,None)))
			case other => UserAccessRestriction(me,validUserAccessesForMe.map(vua => vua.servicePermissions).flatten.toList)
		}
    trace("myUserAccessRestriction: %s".format(myRestriction))
    myRestriction
	}
	def clearValidUsers(): Unit = {
		validUserAccesses = List.empty[UserAccessRestriction]
	}
	def isValidUser: Boolean = validUserAccesses.exists(vua => vua.name == currentUser.is)
  //Globals for the current session
  object casState extends SessionVar[LiftAuthStateData](LiftAuthStateDataForbidden)
  object currentUser extends SessionVar[String](casState.is.username)
	object currentUserAccessRestriction extends SessionVar[UserAccessRestriction](updatedUserAccessRestriction)

  def setupAuthenticators: immutable.Seq[Unit] = {
    val alreadyLoggedInFunc = () => casState.is.authenticated
    val onSuccessFunc:LiftAuthStateData=>Unit = (state:LiftAuthStateData) => {
      casState(state)
      currentUser(state.username)
      currentUserAccessRestriction(updatedUserAccessRestriction)
    }
    for {
      authNodes <- configFile \\ "authentication"
      openId <- authNodes \ "openIdConnect"
      googleClientId <- (openId \ "@googleClientId").headOption.map(_.text)
      googleAppDomain = (openId \ "@googleAppDomain").headOption.map(_.text)
    } yield {
      println("googleClientId: %s".format(googleClientId))
      println("googleAppDomain: %s".format(googleAppDomain))
      LiftAuthAuthentication.attachAuthenticator(new OpenIdConnectAuthenticationSystem(googleClientId, googleAppDomain, alreadyLoggedInFunc, onSuccessFunc))
    }
  }
}
