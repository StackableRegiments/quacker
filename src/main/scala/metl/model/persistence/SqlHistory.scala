package metl.model

import net.liftweb._
import net.liftweb.actor._
import common._
import util._
import Helpers._
import java.util.Date
import net.liftweb.common.Logger

import net.liftweb.json._
import Serialization._

import net.liftweb.mapper.{DB => MapperDB, _}
import net.liftweb.db._

import scala.xml._
import java.sql.Connection

class NamedConnectionIdentifier(override val jndiName: String)
    extends ConnectionIdentifier

object DBVendors {
  def getVendor(driverName: String,
                dbUrl: String,
                dbUser: Box[String],
                dbPassword: Box[String],
                dbSettings: DBSettings = DBSettings()): StandardDBVendor = {
    //new ReconnectingStandardDBVendor(driverName, dbUrl, dbUser, dbPassword, dbSettings)
    new ApacheDbcpStandardDBVendor(driverName,
                                   dbUrl,
                                   dbUser,
                                   dbPassword,
                                   dbSettings)
  }
}
object DBSettingsDefaults {
  val maxPacketSize = Some(5 * 1024 * 1024)
  val allowTemporaryPoolExpansion = true
  val maxPoolSize = 20
  val doNotExpandBeyond = 50
  val maxAttemptsToGetAConnection = Some(20)
  val waitTimeIntervalForConnection = 500L
}

case class DBSettings(
    maxPacketSize: Option[Int] = DBSettingsDefaults.maxPacketSize,
    allowTemporaryPoolExpansion: Boolean =
      DBSettingsDefaults.allowTemporaryPoolExpansion,
    maxPoolSize: Int = DBSettingsDefaults.maxPoolSize,
    doNotExpandBeyond: Int = 50,
    maxAttemptsToGetAConnection: Option[Int] =
      DBSettingsDefaults.maxAttemptsToGetAConnection,
    // I don't know whether it's a bad idea yet to just block up until the database comes back, but maybe it is?  If not, we can set a None here and survive forever, but I think that'll turn out to be a bad idea.
    waitTimeIntervalForConnection: Long =
      DBSettingsDefaults.waitTimeIntervalForConnection,
    connectionTest: Connection => Unit = (conn: Connection) => {
      conn.setAutoCommit(false)
    }
)

class ApacheDbcpStandardDBVendor(
    driverName: String,
    dbUrl: String,
    dbUser: Box[String],
    dbPassword: Box[String],
    dbSettings: DBSettings = DBSettings()
) extends StandardDBVendor(driverName, dbUrl, dbUser, dbPassword)
    with Logger {
  import java.sql.{Connection, DriverManager}
  import org.apache.commons.dbcp2._
  private val lockObject = new Object()
  object DataSource extends BasicDataSource()
  DataSource.setDriverClassName(driverName)
  DataSource.setUrl(dbUrl)
  for {
    u <- dbUser
    p <- dbPassword
  } yield {
    DataSource.setUsername(u)
    DataSource.setPassword(p)
  }
  DataSource.setRemoveAbandonedOnBorrow(true)
  DataSource.setRemoveAbandonedOnMaintenance(true)
  DataSource.setTestOnBorrow(true)
  DataSource.setTestOnReturn(true)
  DataSource.setTestWhileIdle(true)
  DataSource.setMaxTotal(dbSettings.doNotExpandBeyond)
  DataSource.setMaxIdle(dbSettings.maxPoolSize)
  DataSource.setMinIdle(0)
  DataSource.setInitialSize(0)
  DataSource.setMaxWaitMillis(dbSettings.waitTimeIntervalForConnection)
  DataSource.setRemoveAbandonedTimeout(30)
  DataSource.setFastFailValidation(true)

  override def newConnection(name: ConnectionIdentifier): Box[Connection] = {
    val c = tryo(DataSource.getConnection)
    trace(
      "got conn: %s (%s/%s)"
        .format(c, DataSource.getNumActive(), DataSource.getNumIdle()))
    c
  }
  override def releaseConnection(conn: Connection): Unit = {
    conn.close()
    trace(
      "released conn: %s (%s/%s)"
        .format(conn, DataSource.getNumActive(), DataSource.getNumIdle()))
  }
  override def closeAllConnections_!(): Unit = {
    DataSource.close()
  }
}
trait Safely extends Logger {
  protected def safelyOnError(e: Exception): Unit = {}
  protected def safelyOnSuccess(): Unit = {}
  def withDb[A](action: => A): Either[Exception, A] = {
    try {
      val r = Right(action)
      safelyOnSuccess()
      r
    } catch {
      case e: Exception => {
        error("DB threw exception", e)
        safelyOnError(e)
        Left(e)
      }
    }
  }
  def onDb(action: => Unit): Unit = {
    try {
      action
      safelyOnSuccess()
    } catch {
      case e: Exception => {
        error("DB threw exception", e)
        safelyOnError(e)
      }
    }
  }
}

class SqlHistoryListener(override val name: String,
                         driver: String,
                         url: String,
                         username: Option[String],
                         password: Option[String],
                         settings: DBSettings = DBSettings())
    extends PushingToRemoteHistoryListener(name)
    with Safely {
  object LocalConnectionIdentifier
      extends NamedConnectionIdentifier(
        "%s_%s_%s".format(driver, url, username))
  lazy val vendor =
    DBVendors.getVendor(driver, url, username, password, settings)

  object DBCheckResult
      extends DBCheckResult
      with LongKeyedMetaMapper[DBCheckResult] {
    override def dbTableName = "checkresult"
    override def dbDefaultConnectionIdentifier = LocalConnectionIdentifier
    override def createInstance = new DBCheckResult
    override implicit val formats = GraphableData.formats
    def fromCheckResult(in: CheckResult): DBCheckResult = {

      val data = JObject(
        List(
          JField("serviceCheck", JString(in.serviceCheck)),
          JField("label", JString(in.label)),
          JField("serviceLabel", JString(in.serviceLabel)),
          JField("serverLabel", JString(in.serverLabel)),
          JField("why", JString(in.why)),
          JField("detail", JString(in.detail)),
          JField(
            "data",
            JArray(in.data.map(d => {
              JObject(
                List(
                  JField("when", JInt(d._1)),
                  JField(
                    "values",
                    JObject(d._2.toList.map(dTup => {
                      JField(
                        dTup._1,
                        dTup._2 match {
                          case GraphableFloat(f)   => JDouble(f)
                          case GraphableDouble(d)  => JDouble(d)
                          case GraphableInt(i)     => JInt(i)
                          case GraphableLong(l)    => JInt(l)
                          case GraphableString(s)  => JString(s)
                          case GraphableBoolean(b) => JBool(b)
                        }
                      )
                    }))
                  )
                ))
            }))
          )
        ))

      var dbcr = DBCheckResult.createInstance
        .service(in.service)
        .crId(in.id)
        .server(in.server)
        .when(in.when.getTime())
        .mode(in.mode.toString)
        .severity(in.severity.toString)
        .success(in.success)
        .data(write(data))

      dbcr = in.lastUp.map(v => dbcr.lastUp(v.getTime())).getOrElse(dbcr)
      dbcr = in.duration.map(v => dbcr.duration(v)).getOrElse(dbcr)
      dbcr
    }
    def toCheckResult(in: DBCheckResult): CheckResult = {
      val data = parse(in.data.get)
      val JString(serviceCheck) = (data \ "serviceCheck")
      val JString(label) = (data \ "label")
      val JString(serviceLabel) = (data \ "serviceLabel")
      val JString(serverLabel) = (data \ "serverLabel")
      val JString(why) = (data \ "why")
      val JString(detail) = (data \ "detail")
      val parsedData: List[Tuple2[Long, Map[String, GraphableDatum]]] = for {
        d <- (data \ "data").children
        w <- (d \ "when").extractOpt[Long]
        vs <- (d \ "values").extractOpt[Map[String, GraphableDatum]]
      } yield {
        (w, vs)
      }
      CheckResult(
        in.crId.get,
        serviceCheck,
        label,
        in.service.get,
        serviceLabel,
        in.server.get,
        serverLabel,
        new Date(in.when.get),
        why,
        tryo(new Date(in.lastUp.get)),
        detail,
        ServiceCheckMode.parse(in.mode.get),
        ServiceCheckSeverity.parse(in.severity.get),
        in.success.get,
        parsedData,
        tryo(in.duration.get)
      )
    }
  }
  class DBCheckResult extends LongKeyedMapper[DBCheckResult] {
    override def connectionIdentifier = LocalConnectionIdentifier
    implicit val formats = GraphableData.formats
    def getSingleton = DBCheckResult
    def primaryKeyField = id

    object id extends MappedLongIndex(this)
    object service extends MappedString(this, 64) {
      override def dbIndexed_? = true
    }
    object server extends MappedString(this, 64) {
      override def dbIndexed_? = true
    }
    object serviceCheck extends MappedString(this, 256) {
      override def dbIndexed_? = true
    }
    object when extends MappedLong(this) {
      override def dbIndexed_? = true
    }
    object crId extends MappedString(this, 24)
    object lastUp extends MappedLong(this)
    object mode extends MappedString(this, 16)
    object severity extends MappedString(this, 16)
    object success extends MappedBoolean(this)
    object duration extends MappedDouble(this)
    object data extends MappedText(this)
  }

  override def start = {
    super.start
    Class.forName(driver).newInstance()
    DB.defineConnectionManager(LocalConnectionIdentifier, vendor)
    Schemifier.schemify(true,
                        Schemifier.infoF _,
                        LocalConnectionIdentifier,
                        DBCheckResult)
  }
  override def stop = {}
  override def resetEnvironment = {}
  override def getHistoryFor(service: String,
                             server: String,
                             serviceCheck: String,
                             after: Option[Long],
                             before: Option[Long],
                             limit: Option[Int]): List[CheckResult] = {
    val terms: List[QueryParam[DBCheckResult]] = List(
      By(DBCheckResult.service, service),
      By(DBCheckResult.server, server),
      By(DBCheckResult.serviceCheck, serviceCheck)) :::
      after.toList.map(a =>
      Cmp(DBCheckResult.when, OprEnum.>, Full(a), Empty, Empty)) :::
      before.toList.map(b =>
      Cmp(DBCheckResult.when, OprEnum.<, Full(b), Empty, Empty)) :::
      limit.toList.map(l => new MaxRows[DBCheckResult](l.toLong))
    withDb(DBCheckResult.findAll(terms: _*).map(DBCheckResult.toCheckResult _)).right.toOption
      .getOrElse(Nil)
  }
  override def getAllHistory(after: Option[Long],
                             before: Option[Long],
                             limit: Option[Int]): List[CheckResult] = {
    val terms: List[QueryParam[DBCheckResult]] = after.toList.map(a =>
      Cmp(DBCheckResult.when, OprEnum.>, Full(a), Empty, Empty)) :::
      before.toList.map(b =>
      Cmp(DBCheckResult.when, OprEnum.<, Full(b), Empty, Empty)) :::
      limit.toList.map(l => new MaxRows[DBCheckResult](l.toLong))
    withDb(DBCheckResult.findAll(terms: _*).map(DBCheckResult.toCheckResult _)).right.toOption
      .getOrElse(Nil)
  }
  override def performRepeatableAtomicAction(cr: CheckResult): Boolean = {
    withDb(DBCheckResult.fromCheckResult(cr).save).right.toOption
      .getOrElse(false)
  }
}
