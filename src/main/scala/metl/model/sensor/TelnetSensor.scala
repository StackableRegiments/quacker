package metl.model.sensor

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.util.Date

import metl.model.{Sensor, SensorMetaData}
import net.liftweb.util.Helpers._
import org.apache.commons.net.telnet.TelnetClient

class TelnetSensor(metadata: SensorMetaData,
                   host: String,
                   port: Int,
                   time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  protected val commandResponseTerminator: Option[String] = None
  protected def telnetBehaviour(tc: TelnetClient): List[String] = {
    val inputStream = new BufferedInputStream(tc.getInputStream)
    val output = readStream(inputStream)
    inputStream.close
    output.split("\n").toList
  }
  protected def writeTo(input: String, stream: BufferedOutputStream) = {
    val command = (input + "\n").getBytes("ISO-8859-1")
    stream.write(command, 0, command.length)
    stream.flush
  }
  protected def readStream(input: BufferedInputStream,
                           endPattern: Option[String] = None): String =
    readStreamLoop(input, endPattern, new Date(), 5000L)
  protected def readStreamLoop(input: BufferedInputStream,
                               endPattern: Option[String],
                               startTime: Date,
                               timeout: Long): String = {
    var output = ""
    var hasFinished = false
    while (input.available > 0 && !hasFinished) {
      output += input.read.asInstanceOf[Char]
      endPattern.map(ep => {
        hasFinished = output.endsWith(ep)
      })
    }
    if (output == "" && (new Date().getTime - startTime.getTime) < timeout) {
      Thread.sleep(timeout / 10)
      output += readStreamLoop(input, endPattern, startTime, timeout)
    }
    output
  }
  def status = {
    val tc = new TelnetClient()
    tc.connect(host, port)
    val output = telnetBehaviour(tc)
    tc.disconnect
    output.mkString("\n")
  }
  override def performCheck(after:() => Unit) = {
		succeed(status)
		after()
	}
}
