package metl.model.sensor

import metl.model._
import net.liftweb.util.Helpers._
import java.util.Properties

case class SQLResultSet(rows: Map[Int, SQLRow]) {
  def verify(matchers: Map[String, Matcher]): Boolean = {
    !(rows.values.map(r => r.verify(matchers)).exists(m => !m))
  }
}
case class SQLRow(rowNumber: Int, cells: Map[String, SQLCell[Any]]) {
  def verify(matchers: Map[String, Matcher]): Boolean = {
    !(matchers.keys.map(mk => matchers(mk).verify(cells(mk))).exists(m => !m))
  }
}
case class SQLCell[A](name: String, value: A) {
  def verify(matcher: Matcher): Boolean = {
    matcher.verify(value)
  }
}

object VerifiableSQLResultSetConverter {
  def toVerifiableSQLResultSet(resultSet: java.sql.ResultSet): SQLResultSet = {
    val metaData = resultSet.getMetaData
    var rowNumber = 1
    if (resultSet.isBeforeFirst()) {
      resultSet.next()
    }
    var output = List(toRow(rowNumber, resultSet, Some(metaData)))
    while (resultSet.next()) {
      rowNumber = rowNumber + 1
      output = output ::: List(toRow(rowNumber, resultSet, Some(metaData)))
    }
    SQLResultSet(Map(output.map(o => (o.rowNumber, o)): _*))
  }
  protected def toRow(
      rowNumber: Int,
      resultSet: java.sql.ResultSet,
      metaData: Option[java.sql.ResultSetMetaData] = None): SQLRow = {
    val md = metaData.getOrElse(resultSet.getMetaData)
    val output = Map(Range(1, md.getColumnCount() + 1).map(colNumber => {
      val colType = md.getColumnType(colNumber)
      val name = md.getColumnName(colNumber)
      val internalOutput = colType match {
        case /*ARRAY*/ 2003 => SQLCell(name, "not deconstructing ARRAYs")
        case /*BIGINT*/ -5  => SQLCell(name, resultSet.getLong(colNumber))
        case /*BINARY*/ -2  => SQLCell(name, resultSet.getBytes(colNumber))
        case /*BIT*/ -7     => SQLCell(name, resultSet.getInt(colNumber))
        case /*BLOB*/ 2004 => {
          val blob = resultSet.getBlob(colNumber)
          SQLCell(name, blob.getBytes(0, blob.length.toInt))
        }
        case /*BOOLEAN*/ 16  => SQLCell(name, resultSet.getBoolean(colNumber))
        case /*CHAR*/ 1      => SQLCell(name, resultSet.getByte(colNumber))
        case /*CLOB*/ 2005   => SQLCell(name, resultSet.getString(colNumber))
        case /*DATALINK*/ 70 => SQLCell(name, "not deconstructing DATALINKs")
        case /*DATE*/ 91     => SQLCell(name, resultSet.getDate(colNumber).toString)
        case /*DECIMAL*/ 3 =>
          SQLCell(name, resultSet.getBigDecimal(colNumber).toString)
        case /*DISTINCT*/ 2001 => SQLCell(name, "not deconstructing DISTINCTs")
        case /*DOUBLE*/ 8      => SQLCell(name, resultSet.getDouble(colNumber))
        case /*FLOAT*/ 6       => SQLCell(name, resultSet.getFloat(colNumber))
        case /*INTEGER*/ 4     => SQLCell(name, resultSet.getInt(colNumber))
        case /*JAVA_OBJECT*/ 2000 =>
          SQLCell(name, resultSet.getObject(colNumber))
        case /*LONGVARBINARY*/ -4 =>
          SQLCell(name, "not deconstructing LONGVARBINARYs")
        case /*LONGVARCHAR*/ -1 =>
          SQLCell(name, "not deconstructing LONGVARCHARs")
        case /*LONGNVARCHAR*/ -16 =>
          SQLCell(name, "not deconstructing LONGNVARCHARs")
        case /*NCHAR*/ -15   => SQLCell(name, "not deconstructing NCHARs")
        case /*NCLOB*/ 2011  => SQLCell(name, "not deconstructing NCLOBs")
        case /*NULL*/ 0      => SQLCell(name, "SQL NULL returned")
        case /*NUMERIC*/ 2   => SQLCell(name, resultSet.getLong(colNumber))
        case /*NVARCHAR*/ -9 => SQLCell(name, "not deconstructing NVARCHARs")
        case /*OTHER*/ 1111  => SQLCell(name, "not deconstructing OTHERs")
        case /*REAL*/ 7      => SQLCell(name, resultSet.getLong(colNumber))
        case /*REF*/ 2006    => SQLCell(name, "not deconstructing REFs")
        case /*ROWID*/ -8 =>
          SQLCell(name, resultSet.getRowId(colNumber).toString)
        case /*SMALLINT*/ 5   => SQLCell(name, resultSet.getInt(colNumber))
        case /*SQLXML*/ 2009  => SQLCell(name, "not deconstructing SQLXMLs")
        case /*STRUCT*/ 2002  => SQLCell(name, "not deconstructing STRUCTs")
        case /*TIME*/ 92      => SQLCell(name, "not deconstructing TIMEs")
        case /*TIMESTAMP*/ 93 => SQLCell(name, "not deconstructing TIMESTAMPs")
        case /*TINYINT*/ -6   => SQLCell(name, resultSet.getInt(colNumber))
        case /*VARBINARY*/ -3 => SQLCell(name, "not deconstructing VARBINARYs")
        case /*VARCHAR*/ 12   => SQLCell(name, resultSet.getString(colNumber))
        case other =>
          SQLCell(name, "not deconstructing UNKNOWNs (%s)".format(other))
      }
      (name, internalOutput)
    }): _*)
    SQLRow(rowNumber, output.asInstanceOf[Map[String, SQLCell[Any]]])
  }
}

case class VerifiableSqlResultSetDefinition(rowBehaviour: String,
                                            matchers: Map[String, Matcher]) {
  val internalRowBehaviour = rowBehaviour.toLowerCase.trim match {
    case "all" => "all"
    case n: String => {
      try {
        n.toInt.toString
      } catch {
        case e: Throwable => "none"
      }
    }
    case other => "none"
  }
  val verifyRowFunc: SQLRow => Boolean = {
    internalRowBehaviour match {
      case "all" =>
        (a: SQLRow) =>
          true
      case "none" =>
        (a: SQLRow) =>
          false
      case "any" =>
        (a: SQLRow) =>
          true
      case n: String => {
        try {
          val rowNumber = n.toInt
          (a: SQLRow) =>
            rowNumber == a.rowNumber
        } catch {
          case e: Throwable =>
            (a: SQLRow) =>
              false
        }
      }
      case _ =>
        (a: SQLRow) =>
          false
    }
  }
  def verifyResultSet(resultSet: SQLResultSet): VerificationResponse = {
    var errors = List.empty[String]
    var successes = List.empty[SQLRow]
    resultSet.rows.toList.foreach(rsr => {
      val resultSetRow = rsr._2
      val rowNumber = rsr._1
      if (verifyRowFunc(resultSetRow)) {
        matchers.keys.foreach(mk => {
          val pertinentCell = resultSetRow.cells(mk)
          val matcher = matchers(mk)
          if (!pertinentCell.verify(matcher)) {
            errors = errors ::: List(
              "row (%s) (%s) failed verification from row(%s) %s".format(
                rowNumber,
                resultSetRow,
                internalRowBehaviour,
                matcher.describe))
          } else {
            successes = successes ::: List(resultSetRow)
          }
        })
      }
    })
    internalRowBehaviour match {
      case "all" =>
        VerificationResponse(
          resultSet.rows.toList.length > 0 && errors.length == 0,
          errors)
      case "none" =>
        VerificationResponse(
          resultSet.rows.toList.length > 0 && successes.length == 0,
          errors)
      case "some" =>
        VerificationResponse(
          resultSet.rows.toList.length > 0 && successes.length > 0,
          errors)
      case "someNot" =>
        VerificationResponse(
          resultSet.rows.toList.length > 0 && errors.length > 0,
          errors)
      case other =>
        VerificationResponse(
          resultSet.rows.toList.length > 0 && errors.length == 0,
          errors)
    }
  }
}

object OracleSetup {
  Class.forName("oracle.jdbc.OracleDriver").newInstance()
  def initialize = {}
}

class CombinedExceptionsException(exceptions: List[Throwable])
    extends DashboardException(
      "multiple exceptions thrown",
      exceptions.map(e => e.getMessage).mkString("\r\n"),
      exceptions.flatMap(_ match {
        case e: Exception => Some(e)
        case _            => None
      }))

import java.sql.{Connection, DriverManager}
import java.util.concurrent.TimeoutException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

case class OracleSensor(override val metadata: SensorMetaData,
                        uri: String,
                        username: String,
                        password: String,
                        query: String,
                        thresholds: List[VerifiableSqlResultSetDefinition] =
                          List.empty[VerifiableSqlResultSetDefinition],
                        time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  OracleSetup.initialize
  protected val connectionCreationTimeout = 10000L
  override def performCheck = {
    var output = SQLResultSet(Map.empty[Int, SQLRow])
    var errors = List.empty[Throwable]
    try {
      Await.result(
        Future(Some({
          val result = try {
            val conn =
              DriverManager.getConnection("jdbc:oracle:thin:@%s".format(uri),
                                          username,
                                          password)
            val statement = conn.createStatement
            var failedVerificationResponses = List.empty[VerificationResponse]
            val resultSet = statement.executeQuery(query)
            output = VerifiableSQLResultSetConverter.toVerifiableSQLResultSet(
              resultSet)
            resultSet.close
            conn.close
            val verificationResponses =
              thresholds.map(t => t.verifyResultSet(output))
            failedVerificationResponses =
              verificationResponses.filter(pfv => !pfv.success)
            if (failedVerificationResponses.length > 0) {
              errors = errors ::: List(
                new DashboardException("SQL Verification failed",
                                       failedVerificationResponses
                                         .map(fvr => fvr.errors)
                                         .flatten
                                         .mkString("\r\n")))
              (false, conn)
            } else {
              succeed(output.toString)
              (true, conn)
            }
          } catch {
            case e: Throwable => {
              errors = errors ::: List(e)
              (false, null.asInstanceOf[java.sql.Connection])
            }
          }

          /*
        // Fruitless type test: a value of type (Boolean,Connection) cannot also be a List[Option[Option[Tuple2[Boolean,Connection]]]]
        result match {
          case l:List[Option[Option[Tuple2[Boolean,Connection]]]] if l.length > 0 => l.head match {
            case Some(Some((_,null))) => {
              errors = errors ::: List(new DashboardException("SQL Connection failed","connection is null"))
            }
            case Some(Some((true,connection))) => {
            }
            case Some(Some((false,other))) => {
              errors = errors ::: List(new DashboardException("SQL Connection failed","connection: %s".format(other.toString)))
            }
            case other => {
              errors = errors ::: List(new DashboardException("SQL Connection failed","other: %s".format(other.toString)))
            }
          }
        }
         */
        })),
        Duration(connectionCreationTimeout, "millis")
      )
    } catch {
      case e: TimeoutException => {
        errors = errors ::: List(
          new DashboardException(
            "SQL Connection failed",
            "oracle failed to create a connection within the timeout",
            List(e)))
      }
    }
    if (errors.length == 1) {
      throw errors.head
    } else if (errors.length > 0) {
      throw new CombinedExceptionsException(errors)
    }
  }
}

object MySQLSetup {
  Class.forName("com.mysql.jdbc.Driver").newInstance()
  def initialize = {}
}

case class MySQLSensor(override val metadata: SensorMetaData,
                       uri: String,
                       database: String,
                       query: String,
                       username: String,
                       password: String,
                       thresholds: List[VerifiableSqlResultSetDefinition] =
                         List.empty[VerifiableSqlResultSetDefinition],
                       time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  MySQLSetup.initialize
  var sqlConnection: Option[Connection] = None
  protected val connectionCreationTimeout = 3000L
  override def resetEnvironment = {
    sqlConnection.map(_.close)
    sqlConnection = try {
      Await.result(Future(
                     Some(
                       DriverManager.getConnection(
                         "jdbc:mysql://%s/%s".format(uri, database),
                         username,
                         password))),
                   Duration(connectionCreationTimeout, "millis"))
    } catch {
      case e: TimeoutException => {
        error("mysql failed to create a connection within the timeout", e)
        None
      }
    }
  }
  def status = {
    resetEnvironment
    val statementOption = sqlConnection.map(_.createStatement)
    var output = SQLResultSet(Map.empty[Int, SQLRow])
    var failedVerificationResponses = List.empty[VerificationResponse]
    statementOption
      .map(statement => {
        val resultSet = statement.executeQuery(query)
        output =
          VerifiableSQLResultSetConverter.toVerifiableSQLResultSet(resultSet)
        resultSet.close
        val verificationResponses =
          thresholds.map(t => t.verifyResultSet(output))
        failedVerificationResponses =
          verificationResponses.filter(pfv => !pfv.success)
        output
      })
      .getOrElse(
        throw new DashboardException("unable to connect to database", ""))
    sqlConnection.map(_.close)
    if (failedVerificationResponses.length > 0) {
      throw new DashboardException("SQL Verification failed",
                                   failedVerificationResponses
                                     .map(fvr => fvr.errors)
                                     .flatten
                                     .mkString("\r\n"))
    }
    output
  }
  override def performCheck = succeed(status.toString)
}

case class SQLSensor(override val metadata: SensorMetaData,
                     driver: String,
                     url: String,
                     query: String,
                     username: Option[String],
                     password: Option[String],
                     additionalProps: List[Tuple2[String, String]],
                     thresholds: List[VerifiableSqlResultSetDefinition] =
                       List.empty[VerifiableSqlResultSetDefinition],
                     time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  var sqlConnection: Option[Connection] = None
  protected val connectionCreationTimeout = 3000L
  protected lazy val properties = {
    val props = new Properties()
    additionalProps.foreach(prop => {
      props.setProperty(prop._1, prop._2)
    })
    username.foreach(v => props.setProperty("user", v))
    password.foreach(v => props.setProperty("password", v))
    props
  }
  override def resetEnvironment = {
    sqlConnection.map(_.close)
    sqlConnection = try {
      Await.result(Future(Some(DriverManager.getConnection(url, properties))),
                   Duration(connectionCreationTimeout, "millis"))
    } catch {
      case e: TimeoutException => {
        error("mysql failed to create a connection within the timeout", e)
        None
      }
    }
  }
  def status = {
    resetEnvironment
    val statementOption = sqlConnection.map(_.createStatement)
    var output = SQLResultSet(Map.empty[Int, SQLRow])
    var failedVerificationResponses = List.empty[VerificationResponse]
    statementOption
      .map(statement => {
        val resultSet = statement.executeQuery(query)
        output =
          VerifiableSQLResultSetConverter.toVerifiableSQLResultSet(resultSet)
        resultSet.close
        val verificationResponses =
          thresholds.map(t => t.verifyResultSet(output))
        failedVerificationResponses =
          verificationResponses.filter(pfv => !pfv.success)
        output
      })
      .getOrElse(
        throw new DashboardException("unable to connect to database", ""))
    sqlConnection.map(_.close)
    if (failedVerificationResponses.length > 0) {
      throw new DashboardException("SQL Verification failed",
                                   failedVerificationResponses
                                     .map(fvr => fvr.errors)
                                     .flatten
                                     .mkString("\r\n"))
    }
    output
  }
  override def performCheck = succeed(status.toString)
}
