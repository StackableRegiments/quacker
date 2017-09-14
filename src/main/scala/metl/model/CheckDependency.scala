package metl.model

import net.liftweb.util.Helpers._

case class DependencyCheck(serviceCheckMode:ServiceCheckMode,incomingName:String,incomingLabel:String,dependencies:Map[DependencyDescription,DependencyMatcher],time:TimeSpan) extends Pinger(incomingName,incomingLabel,serviceCheckMode){
  override val pollInterval = time
  failureTolerance = 3
  def status = {
    val errors = dependencies.toList.foldLeft(List.empty[String])((acc,item) => {
      val desc = item._1
      val matcher = item._2
      val result = matcher.verify(desc)
      acc ::: result.errors
    })
    if (errors.length > 0)
      throw new DashboardException("DependencyCheck Verification failed",errors.mkString("\r\n"))
    (errors.length == 0).toString
  }
  override def performCheck = succeed(status)
}
