package metl.model.sensor

import java.io.BufferedInputStream

import metl.model.{DashboardException, Sensor, SensorMetaData, ServiceConfigurator}
import net.liftweb.common.{Box, Empty}
import net.liftweb.util.Helpers.{tryo, _}

case class PingICMPSensor(metadata:SensorMetaData, uri:String, ipv6:Boolean = false, time:TimeSpan = 5 seconds) extends Sensor(metadata){
  //pinging is done via ipv4 at present.  ipv6 in some networks results in unexpected results for some subnets
  private val pingCmd = (ServiceConfigurator.isWindows,ServiceConfigurator.isLinux,ServiceConfigurator.isOSX,ipv6) match {
    case (true,false,false,false) => "ping -4 -n 1 "+uri
    case (true,false,false,true) => "ping -6 -n 1 "+uri
    case (false,true,false,false) => "ping -c 1 "+uri
    case (false,true,false,true) => "ping6 -c 1 "+uri
    case (false,false,true,false) => "ping -c 1 "+uri
    case (false,false,true,true) => "ping6 -c 1 "+uri
    case _ => "ping -c 1 "+uri
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

  override val pollInterval = time
  failureTolerance = 3
  def status = {
    val pingProcess = Runtime.getRuntime().exec(pingCmd)
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
    (stringOutput,timeTaken)
  }
  override def performCheck = succeed(status._1,status._2)
}

