package metl.model

import net.liftweb._
import net.liftweb.actor._
import util._
import Helpers._
import java.util.Date
import net.liftweb.common.Logger

import com.mongodb._
import scala.xml._

abstract class HistoryListener(val name:String) extends LiftActor with Logger {
	protected val filterAction:(CheckResult)=>Boolean = (c:CheckResult) => true
	protected def outputAction(cr:CheckResult):Unit
	override def messageHandler = {
		case c:CheckResult if filterAction(c) => outputAction(c)
		case _ => {}
	}
  def getHistoryFor(service:String,server:String,serviceCheck:String,after:Option[Long],limit:Option[Int]):List[CheckResult] = Nil
  def getAllHistory(after:Option[Long],limit:Option[Int]):List[List[CheckResult]] = Nil
}

abstract class PushingToRemoteHistoryListener(name:String) extends HistoryListener(name) {
	protected var pendingActions:List[()=>Boolean] = List.empty[()=>Boolean]
	case object Restart
	def addPendingAction(action:()=>Boolean,tryImmediately:Boolean = true):Unit = {
		pendingActions = pendingActions ::: List(action)	
		if (tryImmediately){
			internalResetEnvironment
		} else {
			Schedule.schedule(this,Restart,1000)
		}
	}
	protected def internalPerformRepeatedAtomicAction(cr:CheckResult):Unit = {
		val action:()=>Boolean = () => performRepeatableAtomicAction(cr)
		if (!action()){
			addPendingAction(action)
		}
	}
	protected def performRepeatableAtomicAction(check:CheckResult):Boolean 
	protected def internalResetEnvironment():Unit = {
		resetEnvironment
		var remainingActions = List.empty[()=>Boolean]
		pendingActions.foreach(pa => {
			if (!pa()){
				remainingActions = remainingActions ::: List(pa)
			}
		})
		pendingActions = remainingActions
	}
	protected def resetEnvironment():Unit = {}
	override def outputAction(cr:CheckResult):Unit = internalPerformRepeatedAtomicAction(cr)
	override def messageHandler: PartialFunction[Any, Unit] = {
		case c:CheckResult if filterAction(c) => outputAction(c)
		case Restart => internalResetEnvironment
		case _ => {}
	}
}

class InMemoryHistoryListener(override val name:String,historyCountPerItem:Int) extends HistoryListener(name) {
  import scala.collection.immutable.Queue
  import scala.collection.mutable.{HashMap => MutMap}
  val store = new MutMap[Tuple3[String,String,String],Queue[CheckResult]]
	override val filterAction:(CheckResult)=>Boolean = (c:CheckResult) => true
	override def outputAction(cr:CheckResult):Unit = {
    val key = (cr.service,cr.server,cr.serviceCheck)
    val oldValue = store.get(key).getOrElse(Queue.empty[CheckResult])
    var newValue = oldValue.enqueue(cr)
    while (historyCountPerItem > 0 && newValue.length > historyCountPerItem && newValue.length > 0){
      val dequeued = newValue.dequeue
      newValue = dequeued._2
    }
    store += ((key,newValue))
  }
  override def getHistoryFor(service:String,server:String,serviceCheck:String,after:Option[Long],limit:Option[Int]):List[CheckResult] = {
    val res = store.get((service,server,serviceCheck)).map(_.toList).getOrElse(Nil)
		val timeBounded = after.map(a => res.filter(_.when.getTime > a)).getOrElse(res)
		limit.map(l => timeBounded.take(l)).getOrElse(timeBounded)
  }
	override def getAllHistory(after:Option[Long],limit:Option[Int]):List[List[CheckResult]] = {
		val all = store.keySet.map(store.get(_).map(_.toList)).map(_.getOrElse(Nil)).toList
		val timeBounded = after.map(a => all.map(c => c.filter(_.when.getTime > a))).getOrElse(all)
		limit.map(l => timeBounded.map(_.take(l))).getOrElse(timeBounded)
	}
}

object NullListener extends HistoryListener("null") {
	override val filterAction:(CheckResult)=>Boolean = (c:CheckResult) => false
	override def outputAction(cr:CheckResult):Unit = {}
}

class DebugHistoryListener(override val name:String) extends HistoryListener(name) {
	override def outputAction(cr:CheckResult):Unit = {
		trace("%s:%s:%s:%s-> %s %s %s:: %s".format(cr.service,cr.server,cr.label,cr.mode,cr.when,cr.when,cr.detail,cr.data.toString.take(10)))	
	}
}


class MongoHistoryListener(override val name:String,host:String,port:Int,database:String,collection:String) extends PushingToRemoteHistoryListener(name){
	var mongo = new MongoClient(host,port)
	//choosing to use the "normal" write concern.  http://api.mongodb.org/java/2.6/com/mongodb/WriteConcern.html for more information
	val defaultWriteConcern: WriteConcern = WriteConcern.valueOf("NORMAL")
	def withMongo[A](action:DBCollection=>A):Option[A] = {
		try {
			val db = mongo.getDB(database)
			val coll = db.getCollection(collection)
			Some(action(coll))
		} catch {
			case e:Throwable => {
				error("failed to write to mongodb (%s:%s/%s/%s)".format(host,port,database,collection),e)
        None
			}
		}	
	}
	override def resetEnvironment = {
		mongo.close
		mongo = new MongoClient(host,port)
	}
	def crToDBObject(cr:CheckResult):DBObject = {
		val dbo = new BasicDBObject
		dbo.put("label",cr.label)
		dbo.put("service",cr.service)
		dbo.put("serviceLabel",cr.serviceLabel)
		dbo.put("server",cr.server)
		dbo.put("serverLabel",cr.serverLabel)
		dbo.put("serviceCheck",cr.serviceCheck)
		dbo.put("when",cr.when)
		dbo.put("why",cr.why)
		cr.lastUp.map(lu => dbo.put("lastUp",lu))
		dbo.put("detail",cr.detail)
		dbo.put("mode",cr.mode.toString)
		dbo.put("severity",cr.severity.toString)
		dbo.put("success",cr.success)
		dbo.put("data",toDBObject(cr.data))
		dbo
	} 
  def dbObjectToCr(dbo:DBObject):CheckResult = {
    CheckResult(
      id = nextFuncName,
      label = dbo.get("label").asInstanceOf[String],
      service = dbo.get("service").asInstanceOf[String],
      serviceLabel = dbo.get("serviceLabel").asInstanceOf[String],
      server = dbo.get("server").asInstanceOf[String],
      serverLabel = dbo.get("serverLabel").asInstanceOf[String],
			serviceCheck = dbo.get("serviceCheck").asInstanceOf[String],
      when = dbo.get("when").asInstanceOf[Date],
      why = dbo.get("why").asInstanceOf[String],
      lastUp = tryo(dbo.get("lastUp").asInstanceOf[Date]),
      detail = dbo.get("detail").asInstanceOf[String],
      mode = ServiceCheckMode.parse(dbo.get("mode").asInstanceOf[String]),
      severity = ServiceCheckSeverity.parse(dbo.get("severity").asInstanceOf[String]),
      success = dbo.get("success").asInstanceOf[Boolean],
      data = Nil
    )
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
				error("unknown dbobject encountered: %s".format(other))
				new BasicDBObject("unknown",other.toString)
			}
		}
	}
  override def getHistoryFor(service:String, server:String, serviceCheck:String, after:Option[Long], limit:Option[Int]):List[CheckResult] = { //not yet implementing "after"
    withMongo(c => {
      val query = new BasicDBObject
      query.put("service",service)
      query.put("server",server)
      query.put("name",serviceCheck)
      c.find(query).toArray
    }).map(_.toArray.toList).getOrElse(Nil).map(o => dbObjectToCr(o.asInstanceOf[DBObject]))
  }
	override def performRepeatableAtomicAction(cr:CheckResult):Boolean = {
		withMongo(c => {
			c.insert(toDBObject(cr).asInstanceOf[DBObject],defaultWriteConcern)
			true
		}).getOrElse(false)
	}
}

object HistoryServer extends LiftActor with ConfigFileReader with Logger {
	var historyListeners:List[HistoryListener] = List.empty[HistoryListener]
	def clear = {
		historyListeners = List.empty[HistoryListener]
	}
  def getHistory(listenerName:Option[String],service:String,server:String,serviceCheckName:String,limit:Option[String]):List[CheckResult] = {
    val listeners = listenerName.map(n => historyListeners.filter(_.name == n)).getOrElse(historyListeners)
		val count = limit match {
			case s:Some[String] => s.map(_.toInt)
			case None => None
		}
    listeners.flatMap(_.getHistoryFor(service, server, serviceCheckName, None, count))
  }
	def getAllHistory(limit:Option[String]):List[List[CheckResult]] = {
		val count = limit match {
			case s:Some[String] => s.map(_.toInt)
			case None => None
		}
		historyListeners.flatMap(_.getAllHistory(None, count))
	}
	def configureFromXml(xml:Node):List[String] = {
		debug("configureFromXml: %s".format(xml))
		val newHistoryListeners = (xml \ "historyListeners").map(hls => (hls \ "historyListener")).flatten.map(n => {
			val name = getText(n,"name").getOrElse("unknown history listener")
			val listenerType = getText(n,"type")
			listenerType match {
        case Some("inMemory") => {
          val queueLength = getInt(n,"queueLength").getOrElse(1)
          new InMemoryHistoryListener(name,queueLength)
        }
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
				case other => {
					error("failed to construct listener from(%s): %s".format(other,n))
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


