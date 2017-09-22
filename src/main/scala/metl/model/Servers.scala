package metl.model

import org.apache.commons.io.IOUtils
import net.liftweb._
import net.liftweb.http._
import net.liftweb.actor._
import net.liftweb.common._
import http.js.JsCmds._
import util._
import Helpers._
import xml._
import S._
import net.liftweb.http.SHtml._
import java.util.Date
import scala.util.Random.shuffle
import collection.JavaConverters._
import net.liftweb.common.Logger
import net.liftweb.util.TimeHelpers
import metl.comet._
import scala.collection.mutable.HashMap

case class ServiceDefinition(name:String,servers:List[ServerDefinition])
case class ServerDefinition(name:String,service:String,checks:List[VisualElement])

object Servers extends ConfigFileReader{
	protected var services = List.empty[ServiceDefinition]
	def clear = {
		services = List.empty[ServiceDefinition]
		rebuildChecks
	}
	def configureFromXml(x:Node):List[String] = {
		var newServicesList = List.empty[ServiceDefinition]
		var newServersList = List.empty[ServerDefinition]
		var newChecksList = List.empty[VisualElement]
		(x \\ "services").foreach(servicesXml => {
			(servicesXml \\ "service").foreach(serviceXml => {
				val serviceName = getAttr(serviceXml,"name").getOrElse("unknown")
				val servicesToStop = services.filter(s => s.name == serviceName)
				servicesToStop.foreach(sts => sts.servers.foreach(s => s.checks.filter(c => c.isInstanceOf[Pinger]).map(c => c.asInstanceOf[Pinger]).foreach(c => c ! StopPinger)))
				val newServers = (serviceXml \ "server").map(serverXml => {
					val serverName = getAttr(serverXml,"name").getOrElse("unknown")
					val serviceChecks = (serverXml \ "serviceCheck").map(serviceCheckXml => ServiceCheckConfigurator.configureFromXml(serviceCheckXml,serviceName,serverName)).toList.flatten.toList
					newChecksList = newChecksList ::: serviceChecks
					ServerDefinition(serverName,serviceName,serviceChecks)
				}).toList
				newServersList = newServersList ::: newServers
				val newService = ServiceDefinition(serviceName,newServers)
				newServicesList = newServicesList ::: List(newService)
				services = services.filterNot(s => s.name == serviceName) ::: List(newService)
			})
			//rebuildChecks
		})
		var output = List.empty[String]
		newServicesList.length match {
			case 0 => {}
			case other => output = output ::: List("loaded %s services".format(other))
		}
		newServersList.length match {
			case 0 => {}
			case other => output = output ::: List("loaded %s servers".format(other))
		}
		newChecksList.length match {
			case 0 => {}
			case other => output = output ::: List("loaded %s checks".format(other))
		}
		output
	}
	def rebuildChecks = {
		val oldChecks = checks
		checks = services.map(service => service.servers).map(servers => servers.map(server => server.checks).flatten.filter(_.isInstanceOf[Pinger]).map(_.asInstanceOf[Pinger])).flatten.toList
		oldChecks.foreach(check => {
			if (!checks.contains(check)){
				check ! StopPinger
//				Schedule.schedule(check,StopPinger,60 seconds)
			}
		})
		checks.foreach(check => {
			if (!check.isRunning){
				check match {
//	this is a thought to ensure that the dependency checks start after the things they depend on.  I'm thinking instead that it would be better to simply set a default sequential required failures on them.
//					case d:DependencyCheck => Schedule.schedule(d,StartPinger,30 seconds)
					case p:Pinger => p ! StartPinger
				}
			}
		})
	}
	protected var checks = List.empty[Pinger] 
  def getVisualElements:List[VisualElement] = {
    val myRestriction = Globals.currentUserAccessRestriction
		println("getVisualElements: %s\r\nPermissions:%s".format(services,myRestriction))
    services.filter(service => myRestriction.permit(service)).map(service => {
      service.servers.filter(server => myRestriction.permit(server)).map(server => {
        server.checks.filter(check => myRestriction.permit(check))
      }).flatten
    }).flatten.toList
  }
	def checksFor(pingerName:String,serviceName:Option[String] = None,serverName:Option[String] = None,serviceCheckMode:Option[ServiceCheckMode]):List[Pinger] = {
		checks.filter(c => {
			c match {
				case p:Pinger => {
					pingerName == p.label && serviceName.map(svcName => svcName == p.serviceName).getOrElse(true) && serverName.map(svrName => svrName == p.serverName).getOrElse(true) && serviceCheckMode.map(svcMode => svcMode == p.mode).getOrElse(true)
				}
				case _ => false
			}
		}).map(ve => ve.asInstanceOf[Pinger]).toList
	}
  def breakSomething(count:Int = 3) = shuffle(checks).take(count).foreach(_.fail("This is a drill"))
}
