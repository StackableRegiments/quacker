package metl.model

import net.liftweb._
import net.liftweb.util.Helpers._

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.sql.{Connection, DriverManager}
import java.util.Date
import javax.naming.Context
import javax.naming.directory.{InitialDirContext, SearchControls}

import GraphableData._
import com.metl.utils.{CleanHttpClient, HTTPResponse, Http}
import net.liftweb.common.{Box, Empty, Full, Logger}
import net.liftweb.util.Helpers.{now, tryo}
import org.apache.commons.net.telnet.TelnetClient

import scala.xml.Node

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeoutException

case class ScriptStepResult(body:String,metaData:Map[String,String] = Map.empty[String,String],statusCode:Int = 0,duration:Double = 0.0)

case class FunctionalCheckReturn(result:ScriptStepResult,duration:Double,updatedEnvironment:Map[String,String],data:List[Tuple2[Long,Map[String,GraphableDatum]]],sqlResult:Option[SQLResultSet] = None,httpResult:Option[HTTPResponse] = None,ldapResults:Option[LdapResults] = None,jmxResults:Option[JmxResults] = None){
  protected def safeDisplay(in:String):String = {
    in match {
      case null => ""
      case s:String if s.length > 100 => s.take(100)
      case s:String => s
    }
  }
  override def toString = {
    "StepResult(%s,%s,%s)".format(safeDisplay(result.body),duration,updatedEnvironment.map(t => (t._1,safeDisplay(t._2))))
  }
}

object ScriptStepResult {
  def empty = ScriptStepResult("")
}
object FunctionalCheckReturn {
  def empty = FunctionalCheckReturn(ScriptStepResult.empty,0.0,Map.empty[String,String],Nil)
}

case class HttpAddBasicAuthorization(domain:String,username:String,password:String) extends FunctionalServiceCheck {
  override protected def innerAct(previousResult:FunctionalCheckReturn,interpolator:Interpolator) = {
    see.foreach(s => {
      s.httpClient.addAuthorization(interpolator.interpolate(domain,previousResult.updatedEnvironment),interpolator.interpolate(username,previousResult.updatedEnvironment),interpolator.interpolate(password,previousResult.updatedEnvironment))
    })
    previousResult
  }
}
case class ICMPFunctionalCheck(uri:String,ipv6:Boolean = false) extends FunctionalServiceCheck {
  //pinging is done via ipv4 at present.  ipv6 in some networks results in unexpected results for some subnets
  protected def pingCmd(url:String) = {
    ((ServiceConfigurator.isWindows,ServiceConfigurator.isLinux,ServiceConfigurator.isOSX,ipv6) match {
      case (true,false,false,false) => "ping -4 -n 1 "
      case (true,false,false,true) => "ping -6 -n 1 "
      case (false,true,false,false) => "ping -c 1 "
      case (false,true,false,true) => "ping6 -c 1 "
      case (false,false,true,false) => "ping -c 1 "
      case (false,false,true,true) => "ping6 -c 1 "
      case _ => "ping -c 1 "
    }) + url
  }
  private val pingTimeExtractor:String=>Box[Double] = (ServiceConfigurator.isWindows,ServiceConfigurator.isLinux,ServiceConfigurator.isOSX) match {
    case (true,false,false) => (output:String) => {
      val reg = """time([=<])([0-9]+)ms""".r
      reg.findFirstMatchIn(output) match {
        case Some(timeMatch) => {
          val timeString = timeMatch.group(2)
          timeMatch.group(1) match {
            case "=" => tryo(timeString.toLong)
            //approximating time to 0.95 of time, if the ping response read "time<1ms" instead of "time=1ms"
            case "<" => tryo(timeString.toLong * 0.95)
          }
        }
        case _ => Empty
      }
    }
    case (false,true,false) => (output:String) => {
      val reg = """time[=<]([0-9.]+) ms""".r
      reg.findFirstMatchIn(output) match {
        case Some(timeMatch) => {
          val timeString = timeMatch.group(1)
          tryo(timeString.toDouble)
        }
        case _ => Empty
      }
    }
    case (false,false,true) => (output:String) => {
      val reg = """time[=<]([0-9.]+) ms""".r
      reg.findFirstMatchIn(output) match {
        case Some(timeMatch) => {
          val timeString = timeMatch.group(1)
          tryo(timeString.toDouble)
        }
        case _ => Empty
      }
    }
    case _ => (output:String) => Empty
  }
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val now = new Date().getTime
    val pingProcess = Runtime.getRuntime().exec(pingCmd(interpolator.interpolate(uri,environment)))
    val inputStream = new BufferedInputStream(pingProcess.getInputStream)
    val errorStream = new BufferedInputStream(pingProcess.getErrorStream)
    var output = ""
    pingProcess.waitFor
    while (errorStream.available > 0 || inputStream.available > 0 )
    {
      while (inputStream.available > 0)
        output += inputStream.read.asInstanceOf[Char]
      while (errorStream.available > 0)
        output += errorStream.read.asInstanceOf[Char]
    }
    pingProcess.destroy
    if (output.length == 0)
      throw DashboardException("Ping failed","ping command failed - no response from OS")
    if (output.contains("cannot resolve") || output.contains("Unknown host") || output.contains("could not find host"))
      throw DashboardException("Ping failed","Unknown host: "+output)
    if (!(output.contains(" 0% packet loss") || output.contains("(0% loss)")))
      throw DashboardException("Ping failed","Packet loss recognised: "+output)
    val stringOutput = output.toString
    val timeTaken = pingTimeExtractor(stringOutput)
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now,Map(
      "checkType" -> "icmp",
      "ipv6" -> ipv6,
      "timeTaken" -> timeTaken.getOrElse(0.0)
    ))
    FunctionalCheckReturn(ScriptStepResult(body = stringOutput,duration = timeTaken.openOr(0)),totalDuration + timeTaken.openOr(0.0),environment,newData :: fcr.data)
  }
}

object JDBCFunctionalCheckDriverInitializations extends Logger {
  protected var initializedDrivers = List.empty[String]
  def initialize(driver:String) = {
    try {
      this.synchronized {
        if (!initializedDrivers.contains(driver)){
          Class.forName(driver).newInstance()
          initializedDrivers = driver :: initializedDrivers
        }
      }
    } catch {
      case e:Exception => {
        error("exception initializing JDBC driver",e)
      }
    }
  }
}

case class JDBCFunctionalCheck(driver:String,url:String,username:String,password:String,query:String,thresholds:List[VerifiableSqlResultSetDefinition] = List.empty[VerifiableSqlResultSetDefinition],connectionCreationTimeout:Long = 10000L) extends FunctionalServiceCheck {
  JDBCFunctionalCheckDriverInitializations.initialize(driver)
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    var output = SQLResultSet(Map.empty[Int,SQLRow])
    var start = new java.util.Date().getTime
    var timeTaken:Box[Double] = Empty
    var errors = List.empty[Throwable]
    try {
      Await.result(Future(Some({
        val result = try {
          val conn = DriverManager.getConnection(url,username,password)//"jdbc:oracle:thin:@%s".format(uri),username,password)
          val statement = conn.createStatement
          var failedVerificationResponses = List.empty[VerificationResponse]
          val resultSet = statement.executeQuery(query)
          output = VerifiableSQLResultSetConverter.toVerifiableSQLResultSet(resultSet)
          resultSet.close
          conn.close
          timeTaken = Full(new java.util.Date().getTime - start)
          val verificationResponses = thresholds.map(t => t.verifyResultSet(output))
          failedVerificationResponses = verificationResponses.filter(pfv => !pfv.success)
          if (failedVerificationResponses.length > 0){
            errors = errors ::: List(new DashboardException("SQL Verification failed",failedVerificationResponses.map(fvr => fvr.errors).flatten.mkString("\r\n")))
            (false,conn)
          } else {
            (true,conn)
          }
        } catch {
          case e:Throwable => {
            timeTaken = Full(new java.util.Date().getTime - start)
            errors = errors ::: List(e)
            (false,null.asInstanceOf[java.sql.Connection])
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
        }*/
      })),Duration(connectionCreationTimeout,"millis"))
    } catch {
      case e:TimeoutException => {
        timeTaken = Full(new java.util.Date().getTime - start)
        errors = errors ::: List(new DashboardException("SQL Connection failed","oracle failed to create a connection within the timeout",List(e)))
      }
    }
    if (errors.length == 1) {
      throw errors.head
    } else if (errors.length > 0) {
      throw new CombinedExceptionsException(errors)
    } else {
      val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now.getTime,Map(("checkType",GraphableString("jdbc")) :: timeTaken.toList.map(tt => {
        ("timeTaken",GraphableDouble(tt))
      }):_*))
      FunctionalCheckReturn(
        result = ScriptStepResult(body = output.toString,duration = timeTaken.openOr(0)),
        duration = totalDuration + timeTaken.openOr(0.0),
        updatedEnvironment = environment,
        sqlResult = Some(output),
        data = newData :: fcr.data
      )
    }
  }
}

case class LdapResults(query:String,base:String,results:List[LdapResult])
case class LdapResult(name:String,attrs:List[LdapAttr])
case class LdapAttr(name:String,values:List[String])

case class LdapFunctionalCheck(host:String,username:String,password:String,searchBase:String,query:String) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val start = new java.util.Date().getTime
    var error:Option[Exception] = None
    val env = new java.util.Hashtable[String,String]()
    env.put(Context.INITIAL_CONTEXT_FACTORY,"com.sun.jndi.ldap.LdapCtxFactory")
    env.put(Context.PROVIDER_URL,interpolator.interpolate(host,environment))
    env.put(Context.SECURITY_AUTHENTICATION,"simple")
    env.put(Context.SECURITY_PRINCIPAL,interpolator.interpolate(username,environment))

    env.put(Context.SECURITY_CREDENTIALS,interpolator.interpolate(password,environment))
    var ctx = new InitialDirContext(env)

    val intQuery = interpolator.interpolate(query,environment)
    val intSearchBase = interpolator.interpolate(searchBase,environment)

    var output = LdapResults(intQuery,intSearchBase,Nil)
    try {

      val controls = new SearchControls
      controls.setSearchScope(SearchControls.SUBTREE_SCOPE)

      ctx = new InitialDirContext(env)
      val results = ctx.search(intSearchBase,intQuery,controls)
      while (results.hasMore){
        val row = results.next()
        val rowAttrs = row.getAttributes().getAll()
        var attrs = List.empty[LdapAttr]
        while (rowAttrs.hasMore){
          rowAttrs.next() match {
            case attr:javax.naming.directory.Attribute => {
              val attrValues = attr.getAll()
              var aValues = List.empty[String]
              while (attrValues.hasMore){
                val value = attrValues.next()
                aValues = aValues ::: List(value.toString)
              }
              attrs = attrs ::: List(LdapAttr(attr.getID(),aValues))
            }
            case _ => {}
          }
        }
        output = output.copy(results = output.results ::: List(LdapResult(row.getNameInNamespace,attrs)))
      }
      results.close
    } catch {
      case e:Exception => error = Some(e)
    } finally {
      ctx.close
    }
    error.foreach(e => throw e)
    val duration = new java.util.Date().getTime - start
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now.getTime,Map(
      "checkType" -> "ldap",
      "timeTaken" -> duration
    ))
    FunctionalCheckReturn(
      result = ScriptStepResult(output.toString,Map.empty[String,String],0,duration.toDouble),
      duration = totalDuration + duration,
      updatedEnvironment = environment,
      ldapResults = Some(output),
      data = newData :: fcr.data
    )
  }
}

case class HttpFunctionalCheck(method:String,url:String,parameters:List[Tuple2[String,String]] = Nil,headers:Map[String,String] = Map.empty[String,String],matcher:HTTPResponseMatcher = HTTPResponseMatchers.empty) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val client = see.map(_.httpClient).getOrElse({
      throw new Exception("no available httpClient")
    })
    val interpolatedHeaders = headers.map(h => (h._1,interpolator.interpolate(h._2,environment)))
    interpolatedHeaders.foreach(h => client.addHttpHeader(h._1,interpolator.interpolate(h._2,environment)))
    val interpolatedUrl = interpolator.interpolate(url,environment)
    val interpolatedParameters = parameters.map(p => (interpolator.interpolate(p._1,environment),interpolator.interpolate(p._2,environment)))
    val innerResponse = method.trim.toLowerCase match {
      case "get" => client.getExpectingHTTPResponse(interpolatedUrl)
      case "post" => client.postFormExpectingHTTPResponse(interpolatedUrl,interpolatedParameters)
      case unsupportedMethod => throw new DashboardException("HTTP method not supported","%s [%s => %s] ([%s => %s],%s)".format(unsupportedMethod,url,interpolatedUrl,parameters,interpolatedParameters,headers))
    }
    val response = client.respondToResponse(innerResponse)
    val verificationResponse = matcher.verify(response)
    if (!verificationResponse.success){
      throw new DashboardException("HTTP Verification failed",verificationResponse.errors.mkString("\r\n"))
    }
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now.getTime,Map(
      "checkType" -> "http",
      "timeTaken" -> response.duration.toDouble,
      "statusCode" -> response.statusCode
    ))
    FunctionalCheckReturn(
      result = ScriptStepResult(response.responseAsString,response.headers,response.statusCode,response.duration.toDouble),
      duration = totalDuration + response.duration,
      updatedEnvironment = environment,
      httpResult = Some(response),
      data = newData :: fcr.data
    )
  }
}

class TelnetFunctionalCheck[A](host:String,port:Int) extends FunctionalServiceCheck {
  protected val commandResponseTerminator:Option[String] = None
  protected def telnetBehaviour(tc:TelnetClient):Tuple2[List[String],Option[A]] = {
    val inputStream = new BufferedInputStream(tc.getInputStream)
    val output = readStream(inputStream)
    inputStream.close
    (output.split("\n").toList,None)
  }
  protected def writeTo(input:String,stream:BufferedOutputStream) = {
    val command = (input + "\n").getBytes("ISO-8859-1")
    stream.write(command,0,command.length)
    stream.flush
  }
  protected def readStream(input:BufferedInputStream,endPattern:Option[String] = None):String = readStreamLoop(input,endPattern,new Date(),5000L)
  protected def readStreamLoop(input:BufferedInputStream,endPattern:Option[String],startTime:Date, timeout:Long):String = {
    var output = ""
    var hasFinished = false
    while (input.available > 0 && !hasFinished){
      output += input.read.asInstanceOf[Char]
      endPattern.map(ep => {
        hasFinished = output.endsWith(ep)
      })
    }
    if (output == "" && (new Date().getTime - startTime.getTime) < timeout){
      Thread.sleep(timeout / 10)
      output += readStreamLoop(input,endPattern,startTime,timeout)
    }
    output
  }
  protected def convert(in:A):Map[String,GraphableDatum] = Map.empty[String,GraphableDatum]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val now = new Date().getTime
    val tc = new TelnetClient()
    tc.connect(interpolator.interpolate(host,fcr.updatedEnvironment),port)
    val output = telnetBehaviour(tc)
    tc.disconnect
    val duration = (new Date().getTime - now).toDouble
    val carrierData = output._2.map(convert _).getOrElse(Map.empty[String,GraphableDatum])
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now,Map(
      "checkType" -> GraphableString("telnet"),
      "timeTaken" -> GraphableDouble(duration),
      "statusCode" -> GraphableInt(200)
    ) ++ carrierData)
    FunctionalCheckReturn(
      result = ScriptStepResult(output._1.mkString("\r\n"),Map.empty[String,String],200,duration),
      duration = fcr.duration + duration,
      updatedEnvironment = fcr.updatedEnvironment,
      data = newData :: fcr.data
    )
  }
}

case class MuninFunctionalCheck(host:String, port:Int, onlyFetch:List[MuninCategoryDefinition] = List(MuninCategoryDefinition("cpu",Counter),MuninCategoryDefinition("memory",Guage))) extends TelnetFunctionalCheck[Map[String,Map[String,Double]]](host,port){
  protected val previous = {
    val map = new MSMap[String,MSMap[String,Double]]()
    onlyFetch.map(munCatDef => map.put(munCatDef.name,new MSMap[String,scala.Double]()))
    map
  }
  override val commandResponseTerminator:Option[String] = Some("\n.\n")
  protected def generatedDelta[Double](inputName:String,input:Map[String,scala.Double]):Map[String,scala.Double] = {
    val result = previous.get(inputName).map(po => Map(input.keys.map(ink => {
      val updatedValue = (po(ink),input(ink)) match {
        case (p:scala.Double,i:scala.Double) if (i < p) => {
          // this is the counter reset behaviour.  I do believe that counters occasionally reset themselves to zero in munin
          debug("possibleOverflow: %s (%s -> %s)".format(ink,p,i))
          val out = i
          po.put(ink,0.0)
          out
        }
        case (p:scala.Double,i:scala.Double) => {
          debug("correct counter behaviour: %s (%s -> %s)".format(ink,p,i))
          val out = i - p
          po.put(ink,i)
          out
        }
        case other => {
          debug("resetting to zero: %s (%s)".format(ink,other))
          val out = 0.0
          po.put(ink,0.0)
          out
        }
      }
      (ink,updatedValue)
    }).toList:_*)).getOrElse(input)
    result
  }
  protected def interpretMuninData(tc:TelnetClient):Map[String,Map[String,Double]] = {
    val outputStream = new BufferedOutputStream(tc.getOutputStream)
    val inputStream = new BufferedInputStream(tc.getInputStream)
    var output = readStream(inputStream)
    if (output.length == 0)
      throw new DashboardException("Munin failed","no response from remote node")
    writeTo("list",outputStream)
    val possibleQueries = readStream(inputStream).split(" ").toList
    val desiredQueries = onlyFetch.filter(of => possibleQueries.contains(of.name))
    val completeOutput = Map(desiredQueries.map(of => {
      val o = of.name
      writeTo("fetch "+o,outputStream)
      val individualOutput = readStream(inputStream,commandResponseTerminator)
      val formattedOutput = Map(individualOutput.split("\n").filter(l => l.length > 1).map(l => {
        val parts = l.split(" ")
        val muninDataKey = parts(0).reverse.dropWhile(c => c != '.').drop(1).reverse.toString
        val muninDataValue = tryo(parts(1).toDouble).openOr(-1.0)
        (muninDataKey,muninDataValue)
      }):_*)
      val finalOutput = of.fieldType match {
        case Counter => {
          try {
            val deltas = generatedDelta(o, formattedOutput)
            Map(deltas.toList.map(fo => (fo._1,fo._2.toDouble)):_*)
          } catch {
            case _: Throwable => formattedOutput
          }
        }
        case Guage => {
          formattedOutput
        }
        case PercentageCounter => {
          try {
            val deltas = generatedDelta(o,formattedOutput)
            val total = deltas.values.sum
            val deltaPercentages = Map(deltas.toList.map(d => (d._1, ((d._2 / total) * 100))):_*)
            deltaPercentages
          } catch {
            case _: Throwable => formattedOutput
          }
        }
        case PercentageGuage => {
          try {
            val total = formattedOutput.values.sum
            Map(formattedOutput.toList.map(fo => (fo._1,((fo._2 / total) * 100))):_*)
          } catch {
            case _: Throwable => formattedOutput
          }
        }
      }
      (o,finalOutput)
    }):_*)
    writeTo("quit",outputStream)
    outputStream.close()
    inputStream.close()
    completeOutput
  }
  override protected def convert(in:Map[String,Map[String,Double]]):Map[String,GraphableDatum] = {
    Map(in.toList.flatMap(ot => {
      ot._2.toList.map(it => {
        ("%s__%s".format(ot._1,it._1),GraphableDouble(it._2))
      })
    }):_*)
  }
  override def telnetBehaviour(tc:TelnetClient):Tuple2[List[String],Option[Map[String,Map[String,Double]]]] = {
    val completeOutput = interpretMuninData(tc)
    (completeOutput.keys.map(cok => "%s -> %s".format(cok,completeOutput(cok))).toList,Some(completeOutput))
  }
}

case class JmxFunctionalCheck(jmxServiceUrl:String,credentials:Option[Tuple2[String,String]] = None) extends FunctionalServiceCheck {
  import java.lang.management._
  import javax.management.remote.{JMXServiceURL,JMXConnectorFactory}
  import collection.JavaConverters._
  override def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val interpolatedUrl = new JMXServiceURL(interpolator.interpolate(jmxServiceUrl,environment))

    val start = new java.util.Date().getTime
    val client = JMXConnectorFactory.connect(interpolatedUrl)
    val beanConnection = client.getMBeanServerConnection()

    val remoteOsBean = ManagementFactory.newPlatformMXBeanProxy(beanConnection,
      ManagementFactory.OPERATING_SYSTEM_MXBEAN_NAME,
      classOf[OperatingSystemMXBean]
    )
    val remoteOs = JmxOsSpec(remoteOsBean.getName,remoteOsBean.getArch,remoteOsBean.getAvailableProcessors,remoteOsBean.getSystemLoadAverage,remoteOsBean.getVersion)
    val remoteRuntimeBean = ManagementFactory.newPlatformMXBeanProxy(beanConnection,
      ManagementFactory.RUNTIME_MXBEAN_NAME,
      classOf[RuntimeMXBean]
    )
    val remoteRuntime = JmxRuntimeSpec(
      remoteRuntimeBean.getName,
      remoteRuntimeBean.getInputArguments.asScala.toList,//.toArray.toList.map(_.asInstanceOf[String]),
      remoteRuntimeBean.getClassPath,
      remoteRuntimeBean.getLibraryPath,
      remoteRuntimeBean.getManagementSpecVersion,
      remoteRuntimeBean.getSpecName,
      remoteRuntimeBean.getSpecVendor,
      remoteRuntimeBean.getSpecVersion,
      remoteRuntimeBean.getSystemProperties.asScala.toMap,
      remoteRuntimeBean.getUptime,
      remoteRuntimeBean.getVmName,
      remoteRuntimeBean.getVmVendor,
      remoteRuntimeBean.getVmVersion,
      remoteRuntimeBean.getStartTime,
      remoteRuntimeBean.isBootClassPathSupported match {
        case true => Some(remoteRuntimeBean.getBootClassPath)
        case false => None
      }
    )
    val remoteMemoryBean = ManagementFactory.newPlatformMXBeanProxy(beanConnection,
      ManagementFactory.MEMORY_MXBEAN_NAME,
      classOf[MemoryMXBean]
    )
    val remoteHeapBean = remoteMemoryBean.getHeapMemoryUsage
    val remoteNonHeapBean = remoteMemoryBean.getNonHeapMemoryUsage
    val remoteMemory = JmxMemorySpec(
      JmxMemoryUsage(
        remoteHeapBean.getInit,
        remoteHeapBean.getUsed,
        remoteHeapBean.getCommitted,
        remoteHeapBean.getMax
      ),
      JmxMemoryUsage(
        remoteNonHeapBean.getInit,
        remoteNonHeapBean.getUsed,
        remoteNonHeapBean.getCommitted,
        remoteNonHeapBean.getMax
      ),
      remoteMemoryBean.getObjectPendingFinalizationCount
    )
    val remoteThreadBean = ManagementFactory.newPlatformMXBeanProxy(beanConnection,
      ManagementFactory.THREAD_MXBEAN_NAME,
      classOf[ThreadMXBean]
    )
    val cpuTimeSupported = remoteThreadBean.isThreadCpuTimeSupported && {
      true
    }
    val allThreadIds = remoteThreadBean.getAllThreadIds
    val cpuTimes = remoteThreadBean.isThreadCpuTimeSupported match {
      case false => Map.empty[Long,Long]
      case true => {
        var enabledThreadCpuTime = false
        if (!remoteThreadBean.isThreadCpuTimeEnabled){
          remoteThreadBean.setThreadCpuTimeEnabled(true)
          enabledThreadCpuTime = true
        }
        val threadTimes = Map(allThreadIds.toList.map(tid => (tid,remoteThreadBean.getThreadCpuTime(tid))):_*)
        if (enabledThreadCpuTime){
          remoteThreadBean.setThreadCpuTimeEnabled(false)
        }
        threadTimes
      }
    }
    val remoteThreads = remoteThreadBean.getThreadInfo(allThreadIds).toList/*.toArray.toList*/.map(ti => {
      JmxThreadSpec(ti.getThreadId,ti.getThreadName,ti.getThreadState,cpuTimes.get(ti.getThreadId))
    })
    val jmxResult = JmxResults(
      remoteOs,
      remoteRuntime,
      remoteMemory,
      remoteThreads
    )
    client.close
    val duration = new java.util.Date().getTime - start
    val newData:Tuple2[Long,Map[String,GraphableDatum]] = (now.getTime,Map(
      "checkType" -> "jmx",
      "timeTaken" -> duration.toDouble,
      "loadAverage" -> remoteOs.loadAverage,
      "uptime" -> remoteRuntime.uptime,
      "startTime" -> remoteRuntime.startTime,
      "heapMax" -> remoteMemory.heap.max,
      "heapUsed" -> remoteMemory.heap.used,
      "nonHeapMax" -> remoteMemory.nonHeap.max,
      "nonHeapUsed" -> remoteMemory.nonHeap.used
    ))
    FunctionalCheckReturn(
      result = ScriptStepResult(jmxResult.toString,Map.empty[String,String],200,duration),
      duration = totalDuration + duration,
      updatedEnvironment = environment,
      jmxResults = Some(jmxResult),
      data = newData :: fcr.data
    )
  }
}

case class JmxOsSpec(name:String,arch:String,processorCount:Int,loadAverage:Double,version:String)
case class JmxRuntimeSpec(name:String,inputArgs:List[String],classPath:String,libraryPath:String,managementSpecVersion:String,specName:String,specVendor:String,specVersion:String,systemProperties:Map[String,String],uptime:Long,vmName:String,vmVendor:String,vmVersion:String,startTime:Long,bootClassPath:Option[String])
case class JmxMemoryUsage(init:Long,used:Long,committed:Long,max:Long)
case class JmxMemorySpec(heap:JmxMemoryUsage,nonHeap:JmxMemoryUsage,objectsPendingFinalizationCount:Int)
case class JmxThreadSpec(threadId:Long,name:String,threadState:java.lang.Thread.State,cpuTime:Option[Long])

case class JmxResults(os:JmxOsSpec,runtime:JmxRuntimeSpec,memory:JmxMemorySpec,threads:List[JmxThreadSpec])

abstract class JmxExtractingEnvironmentMutator extends FunctionalServiceCheck {
  protected def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.jmxResults.map(jmxResult => fcr.copy(updatedEnvironment = mutate(jmxResult,environment,interpolator))).getOrElse(fcr)
  }
}

case class JmxOsLoadExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.loadAverage.toString)
  }
}
case class JmxOsProcessorCountExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.processorCount.toString)
  }
}
case class JmxOsNameExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.name)
  }
}
case class JmxOsArchExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.arch)
  }
}
case class JmxOsVersionExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.os.version)
  }
}

case class JmxRuntimeUptimeExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.runtime.uptime.toString)
  }
}

case class JmxRuntimeInputArgsExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.runtime.inputArgs.mkString(" "))
  }
}

case class JmxRuntimeNameExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.runtime.name)
  }
}
case class JmxHeapMemoryMaxExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.heap.max.toString)
  }
}
case class JmxHeapMemoryUsedExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.heap.used.toString)
  }
}
case class JmxHeapMemoryCommittedExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.heap.committed.toString)
  }
}
case class JmxHeapMemoryInitExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.heap.init.toString)
  }
}
case class JmxHeapMemoryPercentageExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),((result.memory.heap.used * 100) / result.memory.heap.max).toString)
  }
}
case class JmxNonHeapMemoryMaxExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.nonHeap.max.toString)
  }
}
case class JmxNonHeapMemoryUsedExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.nonHeap.used.toString)
  }
}
case class JmxNonHeapMemoryCommittedExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.nonHeap.committed.toString)
  }
}
case class JmxNonHeapMemoryInitExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.memory.nonHeap.init.toString)
  }
}
case class JmxNonHeapMemoryPercentageExtractor(key:String) extends JmxExtractingEnvironmentMutator {
  override def mutate(result:JmxResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),((result.memory.nonHeap.used * 100) / result.memory.nonHeap.max).toString)
  }
}

case class ResultValidator(description:String,validateResult:ScriptStepResult => Boolean) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    if (validateResult(previousResult)){
      fcr
    } else {
      throw new DashboardException("Result failed validation",environment.toString)
    }
  }
}

case class EnvironmentValidator(description:String,validateEnvironment:Map[String,String] => Boolean) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    if (validateEnvironment(environment)){
      fcr
    } else {
      throw new DashboardException("Environment failed validation",environment.toString)
    }
  }
}
abstract class EnvironmentMutator extends FunctionalServiceCheck {
  protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.copy(updatedEnvironment = mutate(previousResult,environment,interpolator))
  }
}
case class LastDataExtractor(key:String,dataAttribute:String) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.data.headOption.flatMap(td => td._2.get(interpolator.interpolate(dataAttribute,environment))).map(nv => {
      fcr.copy(updatedEnvironment = environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(nv.toString,environment)))
    }).getOrElse(fcr)
  }
}
case class LatestDataExtractor(key:String,dataAttribute:String) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.data.flatMap(td => td._2.get(interpolator.interpolate(dataAttribute,fcr.updatedEnvironment)).map(da => (td._1,da.getAsString))).headOption.map(nv => {
      fcr.copy(updatedEnvironment = environment.updated(interpolator.interpolate(key,fcr.updatedEnvironment),interpolator.interpolate(nv._2.toString,fcr.updatedEnvironment)))
    }).getOrElse(fcr)
  }
}

abstract class HttpExtractingEnvironmentMutator extends FunctionalServiceCheck {
  protected def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.httpResult.map(httpResult => fcr.copy(updatedEnvironment = mutate(httpResult,environment,interpolator))).getOrElse(fcr)
  }
}

case class HttpRequestUrlExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.requestUrl.toString,environment))
  }
}
case class HttpStatusCodeExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.statusCode.toString,environment))
  }
}

case class HttpHeaderExtractor(key:String,headerName:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    (for (
      headerValue <- result.headers.get(headerName)
    ) yield {
      environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(headerValue,environment))
    }).getOrElse(environment)
  }
}

case class HttpRedirectCountExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.numberOfRedirects.toString,environment))
  }
}

case class HttpRetryCountExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.numberOfRetries.toString,environment))
  }
}
case class HttpStartTimeExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.startMilis.toString,environment))
  }
}
case class HttpEndTimeExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.endMilis.toString,environment))
  }
}
case class HttpExceptionsExtractor(key:String) extends HttpExtractingEnvironmentMutator {
  protected def stringifyExceptions(in:List[Exception]):String = in.toString
  override def mutate(result:HTTPResponse,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.endMilis.toString,environment))
  }
}
abstract class SqlExtractingEnvironmentMutator extends FunctionalServiceCheck with SafelyExtractFromSql {
  protected def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.sqlResult.map(sqlResult => fcr.copy(updatedEnvironment = mutate(sqlResult,environment,interpolator))).getOrElse(fcr)
  }
}
case class StoreSqlResultSet(key:String) extends SqlExtractingEnvironmentMutator {
  protected def safelyExtract(resultSet:SQLResultSet):String = resultSet.toString
  override def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(safelyExtract(result),environment))
  }
}

trait SafelyExtractFromSql {
  protected val defaultSeparator = ", "
  protected def safelyExtract(cell:SQLCell[_]):String = cell.value.toString
  protected def safelyExtract(row:SQLRow,separator:String):String = row.cells.values.toList.sortWith((a,b) => a.name < b.name).map(c => safelyExtract(c)).mkString(separator)
}

case class SqlRowExtractor(key:String,rowNumber:Int,separator:Option[String] = None) extends SqlExtractingEnvironmentMutator {
  override def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    (for(
      row <- result.rows.get(rowNumber)
    ) yield {
      environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(safelyExtract(row,separator.getOrElse(defaultSeparator)),environment))
    }).getOrElse(environment)
  }
}

case class SqlCellExtractor(key:String,rowNumber:Int,columnName:String) extends SqlExtractingEnvironmentMutator {
  override def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    (for(
      row <- result.rows.get(rowNumber);
      col <- row.cells.get(columnName)
    ) yield {
      environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(safelyExtract(col),environment))
    }).getOrElse(environment)
  }
}
case class SqlColExtractor(key:String,columnName:String,separator:Option[String] = None) extends SqlExtractingEnvironmentMutator {
  override def mutate(result:SQLResultSet,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    val combinedString = result.rows.values.toList.sortWith((a,b) => a.rowNumber < b.rowNumber).flatMap(r => r.cells.get(columnName).map(safelyExtract _)).mkString(separator.getOrElse(defaultSeparator))
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(combinedString,environment))
  }
}

trait SafelyExtractFromLdap {
  val defaultRecordSeparator = "\r\n"
  val defaultAttrSeparator = ", "
  val defaultAttrValueSeparator = " || "
  def stringify(rs:LdapResults):String = rs.results.map(stringify _).mkString(defaultRecordSeparator)
  def stringify(r:LdapResult):String = "Record(%s,%s)".format(r.name,r.attrs.map(stringify _).mkString(defaultAttrSeparator))
  def stringify(r:LdapAttr):String = "Attribute(%s,%s)".format(r.name,r.values.mkString(defaultAttrValueSeparator))
}

abstract class LdapExtractingEnvironmentMutator extends FunctionalServiceCheck with SafelyExtractFromLdap {
  protected def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String]
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.ldapResults.map(ldapResult => fcr.copy(updatedEnvironment = mutate(ldapResult,environment,interpolator))).getOrElse(fcr)
  }
}
case class StoreLdapResults(key:String) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(stringify(result),environment))
  }
}
case class LdapQueryExtractor(key:String) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.query,environment))
  }
}
case class LdapSearchBaseExtractor(key:String) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(result.base,environment))
  }
}
case class LdapRecordExtractor(key:String,recordName:String,separator:Option[String] = None) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    val resultString = result.results.filter(_.name == recordName).map(r => stringify(r)).mkString(separator.getOrElse(defaultRecordSeparator))
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(resultString,environment))
  }
}
case class LdapAttrExtractor(key:String,attrName:String,separator:Option[String] = None) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    val resultString = result.results.flatMap(_.attrs.filter(_.name == attrName)).map(a => stringify(a)).mkString(separator.getOrElse(defaultAttrSeparator))
    environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(resultString,environment))
  }
}
case class LdapAttrFromRecordExtractor(key:String,recordName:String,attrName:String,separator:Option[String] = None) extends LdapExtractingEnvironmentMutator {
  override def mutate(result:LdapResults,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    (for (
      record <- result.results.find(_.name == recordName);
      attr <- record.attrs.find(_.name == attrName)
    ) yield {
      val attrString = attr.values.mkString(separator.getOrElse(defaultAttrValueSeparator))
      environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(attrString,environment))
    }).getOrElse(environment)
  }
}

abstract class ResultMutator extends FunctionalServiceCheck {
  protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):ScriptStepResult
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    fcr.copy(result = mutate(previousResult,environment,interpolator))
  }
}

case class KeySetter(key:String,value:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = environment.updated(interpolator.interpolate(key,environment),interpolator.interpolate(value,environment))
}

case class KeyDeleter(key:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = environment - interpolator.interpolate(key,environment)
}

case class ResultStorer(key:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = environment.updated(interpolator.interpolate(key,environment),result.body)
}
case class MetaDataStorer(key:String,headerName:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    result.metaData.get(headerName).map(newValue => environment.updated(interpolator.interpolate(key,environment),newValue)).getOrElse(environment)
  }
}
case class StatusCodeStorer(key:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment.updated(interpolator.interpolate(key,environment),result.statusCode.toString)
  }
}

case class Cond(key:String, value:String, thenFuncs:List[FunctionalServiceCheck], elseFuncs:List[FunctionalServiceCheck]) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr)
    if (environment.get(interpolator.interpolate(key,environment)).exists(_ == interpolator.interpolate(value,environment))){
      thenFuncs.foreach(tf => {
        state.right.toOption.map(s => {
          state = tf.act(s,interpolator)
        })
      })
    } else {
      elseFuncs.foreach(tf => {
        state.right.toOption.map(s => {
          state = tf.act(s,interpolator)
        })
      })
    }
    state.left.map(e => throw e).right.toOption.get
  }
}

case class WhileLoop(key:String,value:String,funcs:List[FunctionalServiceCheck]) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr)
    while (state.right.toOption.exists(s => s.updatedEnvironment.get(interpolator.interpolate(key,s.updatedEnvironment)).exists(_ == interpolator.interpolate(value,s.updatedEnvironment)))){
      funcs.foreach(tf => {
        state.right.toOption.map(s => {
          state = tf.act(s,interpolator)
        })
      })
    }
    state.left.map(e => throw e).right.toOption.get
  }
}

case class ForLoop(key:String,start:Int,end:Int,incrementing:Boolean,funcs:List[FunctionalServiceCheck]) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    var counter = start
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr.copy(updatedEnvironment = environment.updated(key,counter.toString)))
    if ((incrementing && start < end) || ((!incrementing) && start > end)){
      while (counter != end){
        state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment.updated(interpolator.interpolate(key,s.updatedEnvironment),counter.toString)))
        funcs.foreach(tf => {
          state.right.toOption.map(s => {
            state = tf.act(s,interpolator)
          })
        })
        if (incrementing){
          counter += 1
        } else {
          counter -= 1
        }
      }
    }
    state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment - interpolator.interpolate(key,s.updatedEnvironment)))
    state.left.map(e => throw e).right.toOption.get
  }
}

case class ForeachRegexFromResult(key:String,regex:String,funcs:List[FunctionalServiceCheck]) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val Pattern = interpolator.interpolate(regex,environment).r.unanchored
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr)
    previousResult.body match {
      case Pattern(matches @ _*) => {
        matches.foreach(m => {
          state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment.updated(interpolator.interpolate(key,s.updatedEnvironment),m)))
          funcs.foreach(tf => {
            state.right.toOption.map(s => {
              state = tf.act(s,interpolator)
            })
          })
        })
      }
      case _ => {}
    }
    state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment - interpolator.interpolate(key,s.updatedEnvironment)))
    state.left.map(e => throw e).right.toOption.get
  }
}

case class RegexFromResult(key:String,regex:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    val Pattern = interpolator.interpolate(regex,environment).r.unanchored
    var mutatedEnvironment = environment
    result.body match {
      case Pattern(matches @ _*) => {
        matches.headOption.foreach(firstMatch => {
          mutatedEnvironment = mutatedEnvironment.updated(interpolator.interpolate(key,mutatedEnvironment),firstMatch)
        })
        if (matches.length > 0){
          matches.zipWithIndex.foreach(m => {
            mutatedEnvironment = mutatedEnvironment.updated("%s_%s".format(interpolator.interpolate(key,mutatedEnvironment),m._2),m._1)
          })
        }
      }
      case Pattern(onlyMatch) => {
        mutatedEnvironment = mutatedEnvironment.updated(interpolator.interpolate(key,mutatedEnvironment),onlyMatch)
      }
      case other => {
        throw new DashboardException("Pattern didn't find a valid value: %s ".format(regex),other.toString)
      }
    }
    mutatedEnvironment
  }
}

case class Delay(delay:Long,randomize:Boolean = false) extends FunctionalServiceCheck {
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    val amount:Long = randomize match {
      case true => ((scala.util.Random.nextInt(200) * delay) / 100L)  // pick a value up to twice above the delay value, and down to zero.
      case false => delay
    }
    Thread.sleep(amount)
    fcr
  }
}


case class ForeachXPathFromResult(key:String,xPath:String,funcs:List[FunctionalServiceCheck]) extends FunctionalServiceCheck {
  import org.htmlcleaner._
  override protected def innerAct(fcr:FunctionalCheckReturn,interpolator:Interpolator) = {
    val previousResult = fcr.result
    val totalDuration = fcr.duration
    val environment = fcr.updatedEnvironment
    var state:Either[Exception,FunctionalCheckReturn] = Right(fcr)
    val cleaned = new HtmlCleaner().clean(previousResult.body)
    val matches = cleaned.evaluateXPath(interpolator.interpolate(xPath,environment)).toList.map(_.toString)
    matches.foreach(m => {
      state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment.updated(interpolator.interpolate(key,s.updatedEnvironment),m)))
      funcs.foreach(tf => {
        state.right.toOption.map(s => {
          state = tf.act(s,interpolator)
        })
      })
    })
    state = state.right.map(s => s.copy(updatedEnvironment = s.updatedEnvironment - interpolator.interpolate(key,s.updatedEnvironment)))
    state.left.map(e => throw e).right.toOption.get
  }
}

case class XPathFromResult(key:String,xPath:String) extends EnvironmentMutator {
  import org.htmlcleaner._
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    var mutatedEnvironment = environment
    val cleaned = new HtmlCleaner().clean(result.body)
    val matches = cleaned.evaluateXPath(interpolator.interpolate(xPath,environment)).toList.map(_.toString)
    matches.headOption.foreach(firstMatch => {
      mutatedEnvironment = mutatedEnvironment.updated(interpolator.interpolate(key,mutatedEnvironment),firstMatch)
    })
    if (matches.length > 0){
      matches.zipWithIndex.foreach(m => {
        mutatedEnvironment = mutatedEnvironment.updated("%s_%s".format(interpolator.interpolate(key,mutatedEnvironment),m._2),m._1)
      })
    }
    mutatedEnvironment
  }
}

case class LiftFormExtractor(prefix:String) extends EnvironmentMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):Map[String,String] = {
    environment
  }
}

case class ResultSetter(seed:String) extends ResultMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):ScriptStepResult = ScriptStepResult(interpolator.interpolate(seed,environment))
}

case class RetrieveAttributeToResult(key:String) extends ResultMutator {
  override protected def mutate(result:ScriptStepResult,environment:Map[String,String],interpolator:Interpolator):ScriptStepResult = environment.get(key).map(res => ScriptStepResult(interpolator.interpolate(res,environment))).getOrElse(result)
}

abstract class Interpolator {
  def interpolate(in:String,values:Map[String,String]):String
}
case object EmptyInterpolator extends Interpolator {
  override def interpolate(in:String,values:Map[String,String]):String = in
}
object Interpolator extends ConfigFileReader {
  def configureFromXml(n:Node):Option[Interpolator] = {
    getNodes(n,"interpolator").headOption.flatMap(mn => {
      getAttr(mn,"type").getOrElse("unknown") match {
        case "charKeyedStringInterpolator" => {
          (for (
            startTag <- getAttr(mn,"startTag");
            endTag <- getAttr(mn,"endTag")
          ) yield CharKeyedStringInterpolator(startTag,endTag))
        }
        case "escapedCharKeyedStringInterpolator" => {
          (for (
            startTag <- getAttr(mn,"startTag");
            endTag <- getAttr(mn,"endTag");
            escapeTag <- getAttr(mn,"escapeTag")
          ) yield EscapedCharKeyedStringInterpolator(startTag,endTag,escapeTag))
        }
        case _ => None
      }
    })
  }
}
case class CharKeyedStringInterpolator(startTag:String,endTag:String) extends Interpolator {
  protected val KeyStartTag = startTag
  protected val KeyEndTag = endTag
  def interpolate(in:String,values:Map[String,String]):String = {
    val (partialResult,possibleStartTag,possibleKey,possibleEndTag) = in.foldLeft(("","","",""))((acc,item) => {
      (acc,item) match {
        case ((soFar,KeyStartTag,key,partialEndTag),item) if partialEndTag + item == KeyEndTag => (soFar + values.get(key).getOrElse(KeyStartTag + key + partialEndTag + item),"","","") //end tag complete, commit interpolation
        case ((soFar,KeyStartTag,key,partialEndTag),item) if KeyEndTag.startsWith(partialEndTag + item) => (soFar,KeyStartTag,key,partialEndTag + item) //end tag still building
        case ((soFar,KeyStartTag,key,partialEndTag),item) => (soFar,KeyStartTag,key + partialEndTag + item,"") //start tag complete, key building
        case ((soFar,partialStartTag,key,""),item) if KeyStartTag.startsWith(partialStartTag + item) => (soFar,partialStartTag + item,"","") //start tag still building
        case ((soFar,partialStartTag,key,partialEndTag),item) => (soFar + partialStartTag + key + partialEndTag + item,"","","") //not matched, just adding to the base
      }
    })
    partialResult + possibleStartTag + possibleKey + possibleEndTag
  }
}
case class EscapedCharKeyedStringInterpolator(startTag:String,endTag:String,escapeString:String) extends Interpolator {
  protected val KeyStartTag = startTag
  protected val KeyEndTag = endTag
  protected val EscapeCharacters = escapeString
  def interpolate(in:String,values:Map[String,String]):String = {
    val (partialResult,possibleStartTag,possibleKey,possibleEndTag,escapeChars,escaping) = in.foldLeft(("","","","","",false))((acc,item) => {
      (acc,item) match {
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if escapePattern + item == EscapeCharacters => (soFar,KeyStartTag,key,partialEndTag,"",true) // escapePattern complete - set the next item to escapeMode
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if EscapeCharacters.startsWith(escapePattern + item) => (soFar,KeyStartTag,key,partialEndTag,escapePattern + item,false) // escapePattern building
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,true),item) => (soFar,KeyStartTag,key + partialEndTag + item,"","",false) //character escaped, continuing to build key
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if (partialEndTag + item).length > KeyEndTag.length => (soFar,KeyStartTag,key + partialEndTag + escapePattern + item,"","",false) //end tag became over-long, aborting end-tag and continuing to build key
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if (partialEndTag + escapePattern + item) == KeyEndTag => (soFar + values.get(key).getOrElse(KeyStartTag + key + partialEndTag + item),"","","","",false) //end tag complete, commit interpolation
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) if KeyEndTag.startsWith(partialEndTag + escapePattern + item) => (soFar,KeyStartTag,key,partialEndTag + escapePattern + item,"",false) //end tag still building
        case ((soFar,partialStartTag,"","","",true),item) => (soFar + partialStartTag + item,"","","","",false) //character escaped, aborted building start tag
        case ((soFar,KeyStartTag,key,partialEndTag,escapePattern,false),item) => (soFar,KeyStartTag,key + partialEndTag + escapePattern + item,"","",false) //start tag complete, key building
        case ((soFar,partialStartTag,key,"",escapePattern,false),item) if KeyStartTag == (partialStartTag + item) => (soFar,partialStartTag + escapePattern + item,"","","",false) //start tag complete
        case ((soFar,partialStartTag,key,"",escapePattern,false),item) if (partialStartTag + escapePattern + item).length > KeyStartTag.length => (soFar + partialStartTag + escapePattern + item,"","","","",false) //start tag became over-long, aborting start-tag and adding to base
        case ((soFar,partialStartTag,key,"",escapePattern,false),item) if KeyStartTag.startsWith(partialStartTag + item) => (soFar,partialStartTag + escapePattern + item,"","","",false) //start tag still building
        case ((soFar,partialStartTag,key,partialEndTag,escapePattern,false),item) => (soFar + partialStartTag + key + partialEndTag + escapePattern + item,"","","","",false) //not matched, just adding to the base
      }
    })
    partialResult + possibleStartTag + possibleKey + possibleEndTag + escapeChars + {escaping match {
      case true => escapeString
      case false => ""
    }}
  }
}

class ScriptExecutionEnvironment {
  lazy val httpClient:CleanHttpClient = Http.getClient
}


class ScriptEngine(interpolator:Interpolator) {
  def execute(sequence:List[FunctionalServiceCheck]):FunctionalCheckReturn = {
    val see = new ScriptExecutionEnvironment()
    sequence.foldLeft(FunctionalCheckReturn.empty)((acc,i) => {
      i.attachScriptExecutionEnvironment(see)
      i.act(acc,interpolator) match {
        case Left(e) => {
          throw e
        }
        case Right(fcr) => {
          fcr
        }
      }
    })
  }
}

case class ScriptedCheck(serviceCheckMode:ServiceCheckMode, serviceCheckSeverity:ServiceCheckSeverity, incomingName:String, incomingLabel:String, sequence:List[FunctionalServiceCheck], interpolator:Interpolator, time:TimeSpan) extends Pinger(incomingName,incomingLabel,serviceCheckMode,serviceCheckSeverity){
  override val pollInterval = time
  val scriptEngine = new ScriptEngine(interpolator)
  def status = {
    val fcr = scriptEngine.execute(sequence)
    val finalResult = fcr.result
    val totalDuration = fcr.duration
    val data = fcr.data
    (finalResult,Full(totalDuration),data)
  }
  override def performCheck = succeed(status._1.body,status._2,status._3)
}
