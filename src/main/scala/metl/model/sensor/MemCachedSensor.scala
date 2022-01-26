package metl.model.sensor

import java.net.InetSocketAddress

import metl.model.{Sensor, SensorMetaData}
import net.liftweb.util.Helpers._
import net.spy.memcached.MemcachedClient

case class PingMemCached(metadata: SensorMetaData,
                         uri: String,
                         time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  private val port = 11211
  private val address = new InetSocketAddress(uri, 11211)
  private var cache = new MemcachedClient(address)
  def status = {
    cache.shutdown
    cache = new MemcachedClient(address)
    val stats = cache.getStats
    cache.shutdown
    stats
  }
  override protected def exceptionHandler =
    ({
      case expected: java.util.ConcurrentModificationException => {
        val exceptionMessage =
          "Memcached threw a non-critical exception: %s".format(
            expected.toString)
        succeed(exceptionMessage)
        schedule()
      }
      case other: Throwable => {
        fail(other.toString)
        schedule()
      }
    }: PartialFunction[Throwable, Unit]) orElse super.exceptionHandler
  override def performCheck(after:() => Unit) = {
		succeed(status.toString)
		after()
	}
}
