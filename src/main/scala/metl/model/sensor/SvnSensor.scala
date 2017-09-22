package metl.model.sensor

import metl.model.{Sensor, SensorMetaData}
import net.liftweb.util.Helpers._
import org.tmatesoft.svn.core.SVNURL
import org.tmatesoft.svn.core.internal.io.svn.SVNRepositoryFactoryImpl
import org.tmatesoft.svn.core.io.SVNRepositoryFactory
import org.tmatesoft.svn.core.wc.SVNWCUtil

case class SvnSensor(metadata:SensorMetaData, server:String, username:String, password:String, time:TimeSpan = 5 seconds) extends Sensor(metadata){
  override val pollInterval = time
  SVNRepositoryFactoryImpl.setup
  private val authManager = SVNWCUtil.createDefaultAuthenticationManager(username,password)
  private var repo = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(server),null)
  def status = {
    repo.closeSession
    repo = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(server),null)
    repo.setAuthenticationManager(authManager)
    val logEntries = repo.log(Array.empty[String], null, -1, -1, true, true)
    repo.closeSession
    logEntries.toString
  }
  override def performCheck = succeed(status.toString)
}
