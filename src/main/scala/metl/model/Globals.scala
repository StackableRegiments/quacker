package metl.model

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml._
import scala.collection.mutable.{HashMap, SynchronizedMap, ListBuffer}
import scala.collection.JavaConversions._
import com.mongodb.BasicDBObject
import com.metl.liftAuthenticator._
import com.metl.cas._

object Globals{
  //Globals for the system
  var configDirectoryLocation = "config"
  System.getProperty("quacker.configDirectoryLocation") match {
    case null => throw new Exception("no config directory location passed")
    case other if other.trim.length > 0 => configDirectoryLocation = other
  }
  var isDevMode = false
	protected var validUserAccesses:List[UserAccessRestriction] = List.empty[UserAccessRestriction]
	def setValidUsers(newUsers:List[UserAccessRestriction]) = {
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
    println("myUserAccessRestriction: %s".format(myRestriction))
    myRestriction
	}
	def clearValidUsers = {
		validUserAccesses = List.empty[UserAccessRestriction]
	}
	def isValidUser = validUserAccesses.exists(vua => vua.name == currentUser.is)
  //Globals for the current session
  object casState extends SessionVar[LiftAuthStateData](LiftAuthStateDataForbidden)
  object currentUser extends SessionVar[String](casState.is.username)
	object currentUserAccessRestriction extends SessionVar[UserAccessRestriction](updatedUserAccessRestriction)
}
