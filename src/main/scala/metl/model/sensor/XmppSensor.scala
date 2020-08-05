package metl.model.sensor

import metl.model.{DashboardException, Sensor, SensorMetaData}
import net.liftweb.util.Helpers._
import org.jivesoftware.smack.{
  ConnectionConfiguration,
  SASLAuthentication,
  SmackConfiguration,
  XMPPConnection
}

case class XmppSensor(metadata: SensorMetaData,
                      resource: String,
                      xmppServiceName: Option[String] = None,
                      allowAnonymousAccess: Boolean = false,
                      time: TimeSpan = 10 seconds)
    extends Sensor(metadata) {
  val hostname = resource
  val service = xmppServiceName.getOrElse(hostname)
  override val pollInterval = time
  XMPPConnection.DEBUG_ENABLED = false
  SmackConfiguration.setPacketReplyTimeout(15000)
  SmackConfiguration.setKeepAliveInterval(5000)
  SASLAuthentication.supportSASLMechanism("PLAIN", 0)
  def createConfig = {
    val config = new ConnectionConfiguration(hostname, 5222, service)
    config.setCompressionEnabled(false)
    config.setSendPresence(false)
    config.setRosterLoadedAtLogin(false)
    config.setSelfSignedCertificateEnabled(true)
    config.setSASLAuthenticationEnabled(true)
    config.setReconnectionAllowed(false)
    config.setExpiredCertificatesCheckEnabled(false)
    config.setNotMatchingDomainCheckEnabled(false)
    config.setVerifyChainEnabled(false)
    config.setVerifyRootCAEnabled(false)
    config.setDebuggerEnabled(false)
    config
  }
  private val smackConfig = createConfig
  def createConnection = new XMPPConnection(smackConfig)
  var conn = createConnection
  override protected def exceptionHandler =
    ({
      case expected: Throwable
          if expected.toString.contains("SASL authentication failed") => {
        val exceptionMessage =
          "Xmpp server responded correctly: %s".format(expected.toString)
        succeed(exceptionMessage)
        schedule()
      }
      case other: Throwable => {
        fail(other.toString)
        schedule()
      }
    }: PartialFunction[Throwable, Unit]) orElse super.exceptionHandler
  override def resetEnvironment = {
    if (conn != null && conn.isConnected)
      conn.disconnect
    conn = createConnection
  }
  def status = {
    if (conn != null && conn.isConnected)
      conn.disconnect
    conn.connect
    if (conn.isConnected) {
      conn.login("anillegaluser@".format(service),
                 "shouldnotbeaccepted",
                 new java.util.Date().getTime.toString)
      conn.disconnect
      if (!allowAnonymousAccess)
        throw new DashboardException(
          "XMPP behaved inappropriately",
          "XMPP allowed this user in, and shouldn't have")
      else
        "login succeeded"
    } else {
      throw new DashboardException(
        "XMPP behaved inappropriately",
        "Xmpp connection to %s didn't successfully connect".format(hostname))
    }
  }
  override def performCheck = {
    succeed(status.toString)
  }
}
