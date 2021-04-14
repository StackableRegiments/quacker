package metl.model

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml._
import scala.collection.mutable.{HashMap, SynchronizedMap, ListBuffer}
import scala.collection.JavaConversions._
import com.mongodb.BasicDBObject
import com.metl.cas._
import metl.comet._

object ServicePermission extends ConfigFileReader {
  def configureFromXml(node: Node): ServicePermission = {
    val serviceName = getAttr(node, "name").getOrElse("unknown")
    val whitelistedServiceCheckModes =
      getNodes(node, "allowedServiceCheckModes") match {
        case l: List[Node] if (l.length > 0) =>
          Some(
            l.map(
                ascmn =>
                  getNodes(ascmn, "serviceCheckMode")
                    .map(scmn => getAttr(scmn, "level").getOrElse("unknown"))
                    .filter(a => a != "unknown"))
              .flatten
              .map(scString => ServiceCheckMode.parse(scString)))
        case _ => None
      }
    val blacklistedServiceCheckModes =
      getNodes(node, "disallowedServiceCheckModes") match {
        case l: List[Node] if (l.length > 0) =>
          Some(
            l.map(
                ascmn =>
                  getNodes(ascmn, "serviceCheckMode")
                    .map(scmn => getAttr(scmn, "level").getOrElse("unknown"))
                    .filter(a => a != "unknown"))
              .flatten
              .map(scString => ServiceCheckMode.parse(scString)))
        case _ => None
      }
    val whitelistedServers = getNodes(node, "allowedServers") match {
      case l: List[Node] if (l.length > 0) =>
        Some(
          l.map(
              ascmn =>
                getNodes(ascmn, "server")
                  .map(scmn => getAttr(scmn, "name").getOrElse("unknown"))
                  .filter(a => a != "unknown"))
            .flatten
            .toList)
      case _ => None
    }
    val blacklistedServers = getNodes(node, "disallowedServers") match {
      case l: List[Node] if (l.length > 0) =>
        Some(
          l.map(
              ascmn =>
                getNodes(ascmn, "server")
                  .map(scmn => getAttr(scmn, "name").getOrElse("unknown"))
                  .filter(a => a != "unknown"))
            .flatten
            .toList)
      case _ => None
    }
    new ServicePermission(serviceName,
                          whitelistedServiceCheckModes,
                          blacklistedServiceCheckModes,
                          whitelistedServers,
                          blacklistedServers)
  }
  def empty = PermissiveServicePermission
}

class ServicePermission(
    serviceName: String,
    serviceCheckModeWhitelist: Option[List[ServiceCheckMode]],
    serviceCheckModeBlacklist: Option[List[ServiceCheckMode]],
    serverWhitelist: Option[List[String]],
    serverBlacklist: Option[List[String]]) {
  override def toString: String = {
    "ServicePermission(%s,%s,%s,%s,%s)".format(serviceName,
                                               serviceCheckModeWhitelist,
                                               serviceCheckModeBlacklist,
                                               serverWhitelist,
                                               serverBlacklist)
  }
  def permit(input: AnyRef): Boolean = input match {
    case s: ServiceDefinition => {
      val serviceOkay = serviceName == s.name
      val result = serviceOkay
      result
    }
    case s: ServerDefinition => {
      val serverWhitelisted =
        serverWhitelist.map(sw => sw.contains(s.name)).getOrElse(true)
      val serverBlacklisted =
        serverBlacklist.map(sb => sb.contains(s.name)).getOrElse(false)
      val serviceOkay = serviceName == s.service
      val result = serverWhitelisted && !serverBlacklisted && serviceOkay
      result
    }
    case cr: CheckResult => {
      val serverWhitelisted =
        serverWhitelist.map(sw => sw.contains(cr.server)).getOrElse(true)
      val serverBlacklisted =
        serverBlacklist.map(sb => sb.contains(cr.server)).getOrElse(false)
      val serviceCheckModeWhitelisted = serviceCheckModeWhitelist
        .map(scmw => scmw.contains(cr.mode))
        .getOrElse(true)
      val serviceCheckModeBlacklisted = serviceCheckModeBlacklist
        .map(scmb => scmb.contains(cr.mode))
        .getOrElse(false)
      val serviceOkay = serviceName == cr.service
      serviceCheckModeWhitelisted && !serviceCheckModeBlacklisted && serverWhitelisted && !serverBlacklisted && serviceOkay
    }
    case p: Sensor => {
      val pServerName = p.serverName
      val pServiceName = p.serviceName
      val serverWhitelisted =
        serverWhitelist.map(sw => sw.contains(pServerName)).getOrElse(true)
      val serverBlacklisted =
        serverBlacklist.map(sb => sb.contains(pServerName)).getOrElse(false)
      val serviceCheckModeWhitelisted = serviceCheckModeWhitelist
        .map(scmw => scmw.contains(p.mode))
        .getOrElse(true)
      val serviceCheckModeBlacklisted = serviceCheckModeBlacklist
        .map(scmb => scmb.contains(p.mode))
        .getOrElse(false)
      val serviceOkay = serviceName == pServiceName
      val result = serviceCheckModeWhitelisted && !serviceCheckModeBlacklisted && serverWhitelisted && !serverBlacklisted && serviceOkay
      result
    }
    case ve: VisualElement => {
      val vServerName = ve.serverName
      val vServiceName = ve.serviceName
      val serverWhitelisted =
        serverWhitelist.map(sw => sw.contains(vServerName)).getOrElse(true)
      val serverBlacklisted =
        serverBlacklist.map(sb => sb.contains(vServerName)).getOrElse(false)
      val serviceOkay = serviceName == vServiceName
      serverWhitelisted && !serverBlacklisted && serviceOkay
    }
    case s: StatusCall => {
      val sServerName = s.server
      val sServiceName = s.service
      val sServiceCheckMode = s.serviceCheckMode
      val serverWhitelisted =
        serverWhitelist.map(sw => sw.contains(sServerName)).getOrElse(true)
      val serverBlacklisted =
        serverBlacklist.map(sb => sb.contains(sServerName)).getOrElse(false)
      val serviceCheckModeWhitelisted = serviceCheckModeWhitelist
        .map(scmw => scmw.contains(sServiceCheckMode))
        .getOrElse(true)
      val serviceCheckModeBlacklisted = serviceCheckModeBlacklist
        .map(scmb => scmb.contains(sServiceCheckMode))
        .getOrElse(false)
      val serviceOkay = serviceName == sServiceName
      serviceCheckModeWhitelisted && !serviceCheckModeBlacklisted && serverWhitelisted && !serverBlacklisted && serviceOkay
    }
    case _ => false
  }
}

case object PermissiveServicePermission
    extends ServicePermission("", None, None, None, None) {
  override def permit(input: AnyRef): Boolean = true
}

object UserAccessRestriction extends ConfigFileReader {
  def configureFromXml(node: Node): UserAccessRestriction = {
    val username = getText(node, "authcate").getOrElse("")
    val servicePermissions = getNodes(node, "servicePermissions")
      .map(spNodes =>
        getNodes(spNodes, "service").map(spNode =>
          ServicePermission.configureFromXml(spNode)))
      .flatten
      .toList
    UserAccessRestriction(username, servicePermissions)
  }
  def empty = UserAccessRestriction("empty", List.empty[ServicePermission])
}

case class UserAccessRestriction(name: String,
                                 servicePermissions: List[ServicePermission]) {
  def permit(input: AnyRef): Boolean =
    servicePermissions.length == 0 || servicePermissions.exists(sp =>
      sp.permit(input))
}

object ValidUsers extends ConfigFileReader {
  def configureFromXml(x: Node): List[String] = {
    var output = List.empty[String]
    var newUserNodes = List.empty[UserAccessRestriction]
    getNodes(x, "validUsers").foreach(validUsersNode => {
      val newUsers = getNodes(validUsersNode, "validUser")
        .map(validUserNode =>
          UserAccessRestriction.configureFromXml(validUserNode))
        .filterNot(a => a.name == "")
        .toList
      newUserNodes = newUserNodes ::: newUsers
      Globals.setValidUsers(newUsers)
    })
    newUserNodes.length match {
      case 0     => {}
      case other => output = output ::: List("loaded %s users".format(other))
    }
    output
  }
}
