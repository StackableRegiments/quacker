package metl.model

import org.apache.commons.io.IOUtils
import net.liftweb._
import net.liftweb.actor._
import net.liftweb.common._
import http._
import util._
import Helpers._
import net.liftweb.http.SHtml._
import java.util.Date
import collection.JavaConverters._
import net.liftweb.common.Logger
import net.liftweb.util.TimeHelpers
//file writer
import java.io._

import metl.comet._
import com.mongodb._
import scala.xml._

abstract class HistoryListener(name:String) extends LiftActor {
	protected val filterAction:(CheckResult)=>Boolean = (c:CheckResult) => true
	protected def outputAction(cr:CheckResult):Unit
	override def messageHandler = {
		case c:CheckResult if filterAction(c) => outputAction(c)
		case _ => {}
	}
}

abstract class PushingToRemoteHistoryListener(name:String) extends HistoryListener(name) {
	protected var pendingActions:List[()=>Boolean] = List.empty[()=>Boolean]
	case object Restart
	def addPendingAction(action:()=>Boolean,tryImmediately:Boolean = true):Unit = {
		pendingActions = pendingActions ::: List(action)	
		if (tryImmediately){
			internalResetEnvironment
		} else {
			ActorPing.schedule(this,Restart,1000) 
		}
	}
	protected def internalPerformRepeatedAtomicAction(cr:CheckResult):Unit = {
		val action:()=>Boolean = () => performRepeatableAtomicAction(cr)
		if (!action()){
			addPendingAction(action)
		}
	}
	protected def performRepeatableAtomicAction(check:CheckResult):Boolean 
	protected def internalResetEnvironment:Unit = {
		resetEnvironment
		var remainingActions = List.empty[()=>Boolean]
		pendingActions.foreach(pa => {
			if (!pa()){
				remainingActions = remainingActions ::: List(pa)
			}
		})
		pendingActions = remainingActions
	}
	protected def resetEnvironment:Unit = {}
	override def outputAction(cr:CheckResult):Unit = internalPerformRepeatedAtomicAction(cr)
	override def messageHandler = {
		case c:CheckResult if filterAction(c) => outputAction(c)
		case Restart => internalResetEnvironment
		case _ => {}
	}
}

object NullListener extends HistoryListener("null") {
	override val filterAction:(CheckResult)=>Boolean = (c:CheckResult) => false
	override def outputAction(cr:CheckResult):Unit = {}
}

class DebugHistoryListener(name:String) extends HistoryListener(name) {
	override def outputAction(cr:CheckResult):Unit = {
		println("%s:%s:%s:%s-> %s %s %s:: %s".format(cr.service,cr.server,cr.label,cr.mode,cr.when,cr.when,cr.detail,cr.data.toString.take(10)))	
	}
}


class MongoHistoryListener(name:String,host:String,port:Int,database:String,collection:String) extends PushingToRemoteHistoryListener(name){
	var mongo = new Mongo(host,port)
	//choosing to use the "normal" write concern.  http://api.mongodb.org/java/2.6/com/mongodb/WriteConcern.html for more information
	val defaultWriteConcern = WriteConcern.valueOf("NORMAL")
	def withMongo(action:DBCollection=>Boolean):Boolean = {
		try {
			val db = mongo.getDB(database)
			val coll = db.getCollection(collection)
			action(coll)
		} catch {
			case e:Throwable => {
				println("failed to write to mongodb (%s:%s/%s/%s) for some reason: %s".format(host,port,database,collection,e))
				false
			}
		}	
	}
	override def resetEnvironment = {
		mongo.close
		mongo = new Mongo(host,port)
	}
	def crToDBObject(cr:CheckResult):DBObject = {
		val dbo = new BasicDBObject
		dbo.put("label",cr.label)
		dbo.put("service",cr.service)
		dbo.put("server",cr.server)
		dbo.put("when",cr.when)
		dbo.put("why",cr.why)
		cr.lastUp.map(lu => dbo.put("lastUp",lu))
		dbo.put("detail",cr.detail)
		dbo.put("mode",cr.mode.toString)
		dbo.put("success",cr.success)
		dbo.put("data",toDBObject(cr.data))
		dbo
	} 
	def toDBObject(input:Any,internal:Boolean = false):AnyRef = {
		input match {
			case t:Tuple2[String,Any] => {
				val dbo = new BasicDBObject
				dbo.put(t._1,toDBObject(t._2,true))
				dbo
			}
			case cr:CheckResult => crToDBObject(cr)
			case m:Map[String,Any] => {
				val dbo = new BasicDBObject
				m.keys.foreach(k => dbo.put(k,toDBObject(m(k),true)))
				dbo
			}
			case l:List[Any] => {
				val dbl = new BasicDBList
				l.foreach(li => dbl.add(toDBObject(li,true)))
				dbl
			}
			case s:String if internal => s.asInstanceOf[AnyRef]
			case f:Float if internal => f.asInstanceOf[AnyRef]
			case d:Double if internal => d.asInstanceOf[AnyRef]
			case l:Long if internal => l.asInstanceOf[AnyRef]
			case i:Int if internal => i.asInstanceOf[AnyRef]
			case other => {
				println("unknown dbobject encountered: %s".format(other))
				new BasicDBObject("unknown",other.toString)
			}
		}
	}
	override def performRepeatableAtomicAction(cr:CheckResult):Boolean = {
		withMongo(c => {
			c.insert(toDBObject(cr).asInstanceOf[DBObject],defaultWriteConcern)
			true
		})
	}
}

object HistoryServer extends LiftActor with ConfigFileReader {
	var historyListeners:List[HistoryListener] = List.empty[HistoryListener]
	def clear = {
		historyListeners = List.empty[HistoryListener]
	}
	def configureFromXml(xml:Node):List[String] = {
		val newHistoryListeners = (xml \\ "historyListeners").map(hls => (hls \\ "historyListener")).flatten.map(n => {
			val name = getText(n,"name").getOrElse("unknown history listener")
			val listenerType = getText(n,"type")
			listenerType match {
				case Some("mongodb") => {
					val host = getText(n,"host").getOrElse("localhost")
					val port = getInt(n,"port").getOrElse(27017)
					val db = getText(n,"db").getOrElse("monitoring")
					val collection = getText(n,"collection").getOrElse("rawData")
					val servicePermissions:List[ServicePermission] = getNodes(n,"servicePermissions").map(spNodes => getNodes(spNodes,"service").map(sp => ServicePermission.configureFromXml(sp))).flatten.toList
					val restrictions = UserAccessRestriction(name,servicePermissions)
					val filterFunc = (cr:CheckResult) => restrictions.permit(cr)
					new MongoHistoryListener(name,host,port,db,collection){
						override val filterAction = filterFunc
					}
				}
				case Some("debug") => {
					val servicePermissions:List[ServicePermission] = getNodes(n,"servicePermissions").map(spNodes => getNodes(spNodes,"service").map(sp => ServicePermission.configureFromXml(sp))).flatten.toList
					val restrictions = UserAccessRestriction(name,servicePermissions)
					val filterFunc = (cr:CheckResult) => restrictions.permit(cr)
					new DebugHistoryListener(name){
						override val filterAction = filterFunc
					}
				}
				case _ => {
					println("failed to construct listener from: %s".format(n))
					NullListener
				}
			}
		}).filterNot(hl => hl == NullListener).toList
		newHistoryListeners.foreach(nhl => historyListeners = nhl :: historyListeners)
		newHistoryListeners.length match {
			case 0 => List.empty[String]
			case other => List("loaded %s history listeners".format(other))
		}
	}
	override def messageHandler = {
		case c:CheckResult => {
			historyListeners.foreach(_ ! c)
		}
	}
}


