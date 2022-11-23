package metl.model.sensor

import com.mongodb.MongoClient
import metl.model.{Sensor, SensorMetaData}
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

case class PingMongo(metadata: SensorMetaData,
                     hostname: String,
                     port: Int,
                     database: String,
                     table: String,
                     time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval: Helpers.TimeSpan = time
  var mongo = new MongoClient(hostname, port)
  def status = {
    mongo.close
    mongo = new MongoClient(hostname, port)
    val db = mongo.getDB(database)
    val collection = db.getCollection(table)
    val output = collection.findOne.toMap
    mongo.close
    output
  }
  override def performCheck(after:() => Unit): Unit = {
		succeed(status.toString)
		after()
	}
}
