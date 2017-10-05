package metl.model.sensor

import metl.model._
import net.liftweb.util.Helpers._

case class DependencySensor(metadata:SensorMetaData, dependencies:Map[DependencyDescription,DependencyMatcher], time:TimeSpan) extends Sensor(metadata){
  override val pollInterval = time
  failureTolerance = 3
  def status = {
    val (successes,errors) = dependencies.toList.foldLeft((List.empty[(DependencyDescription,DependencyMatcher)],List.empty[String]))((acc,item) => {
      val desc = item._1
      val matcher = item._2
      val result = matcher.verify(desc)
      if (result.success){
        (acc._1 ::: List((desc,matcher)), acc._2)
      } else {
        (acc._1,acc._2 ::: result.errors)
      }
    })
    if (errors.length > 0) {
      throw new DashboardException("DependencyCheck Verification failed", errors.mkString("\r\n"))
    } else {
      successes.map(s => {
        "(%s_%s_%s) => %s".format(s._1.service.getOrElse(""),s._1.server.getOrElse(""),s._1.pinger,s._2.describe)
      }).mkString(" AND ")
    }
  }
  override def performCheck = succeed(status)
}
