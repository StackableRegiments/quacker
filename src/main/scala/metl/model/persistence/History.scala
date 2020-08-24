package metl.model

import net.liftweb._
import net.liftweb.actor._
import util._
import Helpers._
import java.util.Date
import net.liftweb.common.Logger

import com.mongodb._
import scala.xml._

abstract class HistoryListener(val name: String) extends LiftActor with Logger {
  protected val filterAction: (CheckResult) => Boolean = (c: CheckResult) =>
    true
  protected def outputAction(cr: CheckResult): Unit
  override def messageHandler = {
    case c: CheckResult if filterAction(c) => outputAction(c)
    case _                                 => {}
  }
  def getHistoryFor(service: String,
                    server: String,
                    serviceCheck: String,
                    after: Option[Long],
                    before: Option[Long],
                    limit: Option[Int]): List[CheckResult] = Nil
  def getAllHistory(after: Option[Long],
                    before: Option[Long],
                    limit: Option[Int]): List[CheckResult] = Nil
  def start: Unit = {}
  def stop: Unit = {}
}

abstract class PushingToRemoteHistoryListener(name: String)
    extends HistoryListener(name) {
  protected var pendingActions: List[() => Boolean] = List.empty[() => Boolean]
  case object Restart
  def addPendingAction(action: () => Boolean,
                       tryImmediately: Boolean = true): Unit = {
    pendingActions = pendingActions ::: List(action)
    if (tryImmediately) {
      internalResetEnvironment
    } else {
      Schedule.schedule(this, Restart, 1000)
    }
  }
  protected def internalPerformRepeatedAtomicAction(cr: CheckResult): Unit = {
    val action: () => Boolean = () => performRepeatableAtomicAction(cr)
    if (!action()) {
      addPendingAction(action)
    }
  }
  protected def performRepeatableAtomicAction(check: CheckResult): Boolean
  protected def internalResetEnvironment(): Unit = {
    resetEnvironment
    var remainingActions = List.empty[() => Boolean]
    pendingActions.foreach(pa => {
      if (!pa()) {
        remainingActions = remainingActions ::: List(pa)
      }
    })
    pendingActions = remainingActions
  }
  protected def resetEnvironment(): Unit = {}
  override def outputAction(cr: CheckResult): Unit =
    internalPerformRepeatedAtomicAction(cr)
  override def messageHandler: PartialFunction[Any, Unit] = {
    case c: CheckResult if filterAction(c) => outputAction(c)
    case Restart                           => internalResetEnvironment
    case _                                 => {}
  }
}

class InMemoryHistoryListener(override val name: String,
                              historyCountPerItem: Int)
    extends HistoryListener(name) {
  import scala.collection.immutable.Queue
  import scala.collection.mutable.{HashMap => MutMap}
  val store = new MutMap[Tuple3[String, String, String], Queue[CheckResult]]
  override val filterAction: (CheckResult) => Boolean = (c: CheckResult) => true
  override def outputAction(cr: CheckResult): Unit = {
    val key = (cr.service, cr.server, cr.serviceCheck)
    val oldValue = store.get(key).getOrElse(Queue.empty[CheckResult])
    var newValue = oldValue.enqueue(cr)
    while (historyCountPerItem > 0 && newValue.length > historyCountPerItem && newValue.length > 0) {
      val dequeued = newValue.dequeue
      newValue = dequeued._2
    }
    store += ((key, newValue))
  }
  override def getHistoryFor(service: String,
                             server: String,
                             serviceCheck: String,
                             after: Option[Long],
                             until: Option[Long],
                             limit: Option[Int]): List[CheckResult] = {
    val all =
      store.get((service, server, serviceCheck)).map(_.toList).getOrElse(Nil)
    val timeBoundedAfter =
      after.map(a => all.filter(_.when.getTime > a)).getOrElse(all)
    val timeBoundedBefore = until
      .map(u => timeBoundedAfter.filter(_.when.getTime < u))
      .getOrElse(timeBoundedAfter)
    limit.map(l => timeBoundedBefore.take(l)).getOrElse(timeBoundedBefore)
  }
  override def getAllHistory(after: Option[Long],
                             until: Option[Long],
                             limit: Option[Int]): List[CheckResult] = {
    val all: List[CheckResult] = store.keySet.toList.flatMap(k =>
      store.get(k).map(_.toList).getOrElse(Nil))
    val timeBoundedAfter =
      after.map(a => all.filter(_.when.getTime > a)).getOrElse(all)
    val timeBoundedBefore = until
      .map(u => timeBoundedAfter.filter(_.when.getTime < u))
      .getOrElse(timeBoundedAfter)
    limit.map(l => timeBoundedBefore.take(l)).getOrElse(timeBoundedBefore)
  }
}

object NullListener extends HistoryListener("null") {
  override val filterAction: (CheckResult) => Boolean = (c: CheckResult) =>
    false
  override def outputAction(cr: CheckResult): Unit = {}
}

class DebugHistoryListener(override val name: String)
    extends HistoryListener(name) {
  override def outputAction(cr: CheckResult): Unit = {
    trace(
      "%s:%s:%s:%s-> %s %s %s:: %s".format(cr.service,
                                           cr.server,
                                           cr.label,
                                           cr.mode,
                                           cr.when,
                                           cr.when,
                                           cr.detail,
                                           cr.data.toString.take(10)))
  }
}

object HistoryServer extends LiftActor with ConfigFileReader with Logger {
  var historyListeners: List[HistoryListener] = List.empty[HistoryListener]
  def clear = {
    historyListeners = List.empty[HistoryListener]
  }
  def getHistory(listenerName: Option[String],
                 service: String,
                 server: String,
                 serviceCheckName: String,
                 since: Option[Long],
                 until: Option[Long],
                 limit: Option[Int]): List[CheckResult] = {
    val listeners = listenerName
      .map(n => historyListeners.filter(_.name == n))
      .getOrElse(historyListeners)
    listeners.flatMap(
      _.getHistoryFor(service, server, serviceCheckName, since, until, limit))
  }
  def getAllHistory(since: Option[Long],
                    until: Option[Long],
                    limit: Option[Int]): List[CheckResult] = {
    historyListeners.flatMap(_.getAllHistory(since, until, limit))
  }
  protected def printException(e: Exception): Unit = {
    println("exception during sql load: %s: %s".format(e, e.getMessage))
    e.printStackTrace
    if (e.getCause != null && e.isInstanceOf[Exception]) {
      println("caused by")
      printException(e.getCause.asInstanceOf[Exception])
    }
  }
  def configureFromXml(xml: Node): List[String] = {
    debug("configureFromXml: %s".format(xml))
    val newHistoryListeners = (xml \ "historyListeners")
      .map(hls => (hls \ "historyListener"))
      .flatten
      .map(n => {
        val name = getText(n, "name").getOrElse("unknown history listener")
        val listenerType = getText(n, "type")
        listenerType match {
          case Some("inMemory") => {
            val queueLength = getInt(n, "queueLength").getOrElse(1)
            new InMemoryHistoryListener(name, queueLength)
          }
          case Some("sql") => {
            try {
              val driver = getText(n, "driver").getOrElse("org.h2.Driver")
              val url = getText(n, "url").getOrElse(
                "jdbc:h2:local_quacker.db;AUTO_SERVER=TRUE")
              val username = getText(n, "username")
              val password = getText(n, "password")
              val servicePermissions: List[ServicePermission] =
                getNodes(n, "servicePermissions")
                  .map(spNodes =>
                    getNodes(spNodes, "service").map(sp =>
                      ServicePermission.configureFromXml(sp)))
                  .flatten
                  .toList
              val restrictions =
                UserAccessRestriction(name, name, servicePermissions)
              val filterFunc = (cr: CheckResult) => restrictions.permit(cr)
              new SqlHistoryListener(name, driver, url, username, password) {
                override val filterAction = filterFunc
              }
            } catch {
              case e: Exception => {
                printException(e)
                throw e
              }
            }
          }

          case Some("mongodb") => {
            val host = getText(n, "host").getOrElse("localhost")
            val port = getInt(n, "port").getOrElse(27017)
            val db = getText(n, "db").getOrElse("monitoring")
            val collection = getText(n, "collection").getOrElse("rawData")
            val servicePermissions: List[ServicePermission] =
              getNodes(n, "servicePermissions")
                .map(spNodes =>
                  getNodes(spNodes, "service").map(sp =>
                    ServicePermission.configureFromXml(sp)))
                .flatten
                .toList
            val restrictions =
              UserAccessRestriction(name, name, servicePermissions)
            val filterFunc = (cr: CheckResult) => restrictions.permit(cr)
            new MongoHistoryListener(name, host, port, db, collection) {
              override val filterAction = filterFunc
            }
          }
          case Some("debug") => {
            val servicePermissions: List[ServicePermission] =
              getNodes(n, "servicePermissions")
                .map(spNodes =>
                  getNodes(spNodes, "service").map(sp =>
                    ServicePermission.configureFromXml(sp)))
                .flatten
                .toList
            val restrictions =
              UserAccessRestriction(name, name, servicePermissions)
            val filterFunc = (cr: CheckResult) => restrictions.permit(cr)
            new DebugHistoryListener(name) {
              override val filterAction = filterFunc
            }
          }
          case other => {
            error("failed to construct listener from(%s): %s".format(other, n))
            NullListener
          }
        }
      })
      .filterNot(hl => hl == NullListener)
      .toList
    newHistoryListeners.foreach(nhl => {
      nhl.start
      historyListeners = nhl :: historyListeners
    })
    newHistoryListeners.length match {
      case 0     => List.empty[String]
      case other => List("loaded %s history listeners".format(other))
    }
  }
  override def messageHandler = {
    case c: CheckResult => {
      historyListeners.foreach(_ ! c)
    }
  }
}
