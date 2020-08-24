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

import net.liftweb.json._

object ServicePermission extends ConfigFileReader with JsonReader {
  def configureFromJson(jo: JObject): ServicePermission = {
    val serviceName = asString(jo \ "name").getOrElse("unknown")
    val whitelistedServiceCheckModes = optOnKey(
      jo,
      "allowedServiceCheckModes",
      v => asArrayOfStrings(v).map(ServiceCheckMode.parse _))
    val blacklistedServiceCheckModes = optOnKey(
      jo,
      "disallowedServiceCheckModes",
      v => asArrayOfStrings(v).map(ServiceCheckMode.parse _))
    val whitelistedServers =
      optOnKey(jo, "allowedServers", v => asArrayOfStrings(v))
    val blacklistedServers =
      optOnKey(jo, "disallowedServers", v => asArrayOfStrings(v))
    new ServicePermission(serviceName,
                          whitelistedServiceCheckModes,
                          blacklistedServiceCheckModes,
                          whitelistedServers,
                          blacklistedServers)
  }
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
  val PermissiveServicePermission =
    new ServicePermission("", None, None, None, None) {
      override def permit(input: AnyRef): Boolean = true
    }
  def empty = PermissiveServicePermission
}

case class ServicePermission(
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

object UserAccessRestriction extends ConfigFileReader with JsonReader {
  def configureFromJson(jv: JValue): UserAccessRestriction = {
    jv match {
      case jo: JObject => {
        val id = asString(jo \ "id").getOrElse("")
        val username = asString(jo \ "authcate").getOrElse("")
        val servicePermissions = asArrayOfObjs(jo \ "services").map(c =>
          ServicePermission.configureFromJson(c))
        UserAccessRestriction(id, username, servicePermissions)
      }
      case _ => empty
    }
  }
  def configureFromXml(node: Node): UserAccessRestriction = {
    val id = getText(node, "id").getOrElse("")
    val username = getText(node, "authcate").getOrElse("")
    val servicePermissions = getNodes(node, "servicePermissions")
      .map(spNodes =>
        getNodes(spNodes, "service").map(spNode =>
          ServicePermission.configureFromXml(spNode)))
      .flatten
      .toList
    UserAccessRestriction(id, username, servicePermissions)
  }
  def empty =
    UserAccessRestriction("empty", "empty", List.empty[ServicePermission])
}

case class UserAccessRestriction(id: String,
                                 name: String,
                                 servicePermissions: List[ServicePermission]) {
  def permit(input: AnyRef): Boolean =
    servicePermissions.length == 0 || servicePermissions.exists(sp =>
      sp.permit(input))
}

object ValidUsers extends ConfigFileReader with JsonReader {
  def configureFromJson(n: JValue): List[UserAccessRestriction] = {
    n match {
      case JArray(items) => items.flatMap(configureFromJson _)
      case jo: JObject   => List(UserAccessRestriction.configureFromJson(jo))
      case _             => Nil
    }
  }
  def configureFromXml(x: Node): List[UserAccessRestriction] = {
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
    newUserNodes
  }
}
