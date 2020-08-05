package metl.model.sensor

import java.net.URI

import jcifs.smb.{NtlmPasswordAuthentication, SmbFile}
import metl.model.{DashboardException, Sensor, SensorMetaData}
import net.liftweb.util.Helpers._

case class SambaSensor(metadata: SensorMetaData,
                       hostname: String,
                       domain: String,
                       filename: String,
                       username: String,
                       password: String,
                       time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  def isForURI(uri: URI): Boolean =
    uri.getHost == hostname && uri.getScheme == "smb"
  val auth = new NtlmPasswordAuthentication(domain, username, password)
  val path = filename match {
    case fp if fp.startsWith("smb://") && isForURI(new URI(fp)) => fp
    case fp if fp.startsWith("smb://") =>
      throw new Exception("path requested which is not for this connection")
    case p => "smb://%s/%s".format(hostname, p)
  }
  def status = {
    val file = new SmbFile(path, auth)
    if (file.isFile) {
      "File exists: %s, size: %s, lastModified: %s".format(
        file.exists,
        file.getContentLength,
        file.lastModified)
    } else {
      val fileList = file.list() match {
        case a: Array[String] if a.length == 0 =>
          throw new DashboardException("Samba exception", "resource empty")
        case a: Array[String] => a.toList
        case null =>
          throw DashboardException("Samba exception",
                                   "resource cannot be resolved")
        case other =>
          throw DashboardException(
            "Samba exception",
            "unexpected response from SmbFile.list => %s".format(other))
      }
      fileList.mkString(", ")
    }
  }
  override def performCheck = succeed(status.toString)
}
