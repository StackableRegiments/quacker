package metl.model

import net.liftweb._
import net.liftweb.actor._
import util._
import Helpers._
import java.util.Date
import net.liftweb.common.Logger

import com.mongodb._
import scala.xml._

class MongoHistoryListener(override val name: String,
                           host: String,
                           port: Int,
                           database: String,
                           collection: String)
    extends PushingToRemoteHistoryListener(name) {
  var mongo = new MongoClient(host, port)
  //choosing to use the "normal" write concern.  http://api.mongodb.org/java/2.6/com/mongodb/WriteConcern.html for more information
  val defaultWriteConcern: WriteConcern = WriteConcern.valueOf("NORMAL")
  def withMongo[A](action: DBCollection => A): Option[A] = {
    try {
      val db = mongo.getDB(database)
      val coll = db.getCollection(collection)
      Some(action(coll))
    } catch {
      case e: Throwable => {
        error("failed to write to mongodb (%s:%s/%s/%s)".format(host,
                                                                port,
                                                                database,
                                                                collection),
              e)
        None
      }
    }
  }
  override def resetEnvironment = {
    mongo.close
    mongo = new MongoClient(host, port)
  }
  def crToDBObject(cr: CheckResult): DBObject = {
    val dbo = new BasicDBObject
    dbo.put("label", cr.label)
    dbo.put("service", cr.service)
    dbo.put("serviceLabel", cr.serviceLabel)
    dbo.put("server", cr.server)
    dbo.put("serverLabel", cr.serverLabel)
    dbo.put("serviceCheck", cr.serviceCheck)
    dbo.put("when", cr.when)
    dbo.put("why", cr.why)
    cr.lastUp.map(lu => dbo.put("lastUp", lu))
    dbo.put("detail", cr.detail)
    dbo.put("mode", cr.mode.toString)
    dbo.put("severity", cr.severity.toString)
    dbo.put("success", Boolean.box(cr.success))
    dbo.put("data", toDBObject(cr.data))
    dbo
  }
  def dbObjectToCr(dbo: DBObject): CheckResult = {
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
      severity =
        ServiceCheckSeverity.parse(dbo.get("severity").asInstanceOf[String]),
      success = dbo.get("success").asInstanceOf[Boolean],
      data = Nil
    )
  }
  def toDBObject(input: Any, internal: Boolean = false): AnyRef = {
    input match {
      case t: Tuple2[String, Any] => {
        val dbo = new BasicDBObject
        dbo.put(t._1, toDBObject(t._2, true))
        dbo
      }
      case cr: CheckResult => crToDBObject(cr)
      case m: Map[String, Any] => {
        val dbo = new BasicDBObject
        m.keys.foreach(k => dbo.put(k, toDBObject(m(k), true)))
        dbo
      }
      case l: List[Any] => {
        val dbl = new BasicDBList
        l.foreach(li => dbl.add(toDBObject(li, true)))
        dbl
      }
      case s: String if internal  => s.asInstanceOf[AnyRef]
      case f: Float if internal   => f.asInstanceOf[AnyRef]
      case d: Double if internal  => d.asInstanceOf[AnyRef]
      case l: Long if internal    => l.asInstanceOf[AnyRef]
      case b: Boolean if internal => b.asInstanceOf[AnyRef]
      case i: Int if internal     => i.asInstanceOf[AnyRef]
      case other => {
        error("unknown dbobject encountered: %s".format(other))
        new BasicDBObject("unknown", other.toString)
      }
    }
  }
  override def getHistoryFor(service: String,
                             server: String,
                             serviceCheck: String,
                             after: Option[Long],
                             before: Option[Long],
                             limit: Option[Int])
    : List[CheckResult] = { //not yet implementing "after" or "before"
    withMongo(c => {
      val query = new BasicDBObject
      query.put("service", service)
      query.put("server", server)
      query.put("name", serviceCheck)
      c.find(query).toArray
    }).map(_.toArray.toList)
      .getOrElse(Nil)
      .map(o => dbObjectToCr(o.asInstanceOf[DBObject]))
  }
  override def performRepeatableAtomicAction(cr: CheckResult): Boolean = {
    withMongo(c => {
      c.insert(toDBObject(cr).asInstanceOf[DBObject], defaultWriteConcern)
      true
    }).getOrElse(false)
  }
}
