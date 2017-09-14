package metl.model

import com.mongodb.MongoClient
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

case class PingMongo(serviceCheckMode:ServiceCheckMode,incomingName:String,incomingLabel:String,hostname:String,port:Int,database:String,table:String,time:TimeSpan = 5 seconds) extends Pinger(incomingName,incomingLabel,serviceCheckMode){
  override val pollInterval: Helpers.TimeSpan = time
  var mongo = new MongoClient(hostname,port)
  def status = {
    mongo.close
    mongo = new MongoClient(hostname,port)
    val db = mongo.getDB(database)
    val collection = db.getCollection(table)
    val output = collection.findOne.toMap
    mongo.close
    output
  }
  override def performCheck: Unit = succeed(status.toString)
}
