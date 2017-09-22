package metl.model.sensor

import java.io.{BufferedInputStream, BufferedOutputStream}

import metl.model._
import net.liftweb.util.Helpers.{tryo, _}
import org.apache.commons.net.telnet.TelnetClient

class MSMap[A,B](defaultFunc:A=>B = (a:A) => null.asInstanceOf[B]) extends scala.collection.mutable.HashMap[A,B] with scala.collection.mutable.SynchronizedMap[A,B]{
  override def default(key:A):B = defaultFunc(key)
}
abstract class MuninFieldType {}
object MuninFieldType {
  def parse(input:String):MuninFieldType = input.toLowerCase.trim match {
    case "counter" => Counter
    case "guage" => Guage
    case "percentagecounter" => PercentageCounter
    case "counteraspercentage" => PercentageCounter
    case "percentageguage" => PercentageGuage
    case "guageaspercentage" => PercentageGuage
    case _ => Counter
  }
}
case object Counter extends MuninFieldType
case object Guage extends MuninFieldType
case object PercentageCounter extends MuninFieldType
case object PercentageGuage extends MuninFieldType

case class MuninCategoryDefinition(name:String,fieldType:MuninFieldType,matchers:Map[String,Matcher] = Map.empty[String,Matcher])

class MuninSensor(metadata:SensorMetaData, host:String, port:Int, onlyFetch:List[MuninCategoryDefinition] = List(MuninCategoryDefinition("cpu",Counter),MuninCategoryDefinition("memory",Guage)), time:TimeSpan = 5 seconds) extends TelnetSensor(metadata,host,port,time){
  protected val previous = {
    val map = new MSMap[String,MSMap[String,Double]]()
    onlyFetch.map(munCatDef => map.put(munCatDef.name,new MSMap[String,scala.Double]()))
    map
  }
  override val commandResponseTerminator:Option[String] = Some("\n.\n")
  protected def generatedDelta[Double](inputName:String,input:Map[String,scala.Double]):Map[String,scala.Double] = {
    val result = previous.get(inputName).map(po => Map(input.keys.map(ink => {
      val updatedValue = (po(ink),input(ink)) match {
        case (p:scala.Double,i:scala.Double) if (i < p) => {
          // this is the counter reset behaviour.  I do believe that counters occasionally reset themselves to zero in munin
          debug("possibleOverflow: %s (%s -> %s)".format(ink,p,i))
          val out = i
          po.put(ink,0.0)
          out
        }
        case (p:scala.Double,i:scala.Double) => {
          debug("correct counter behaviour: %s (%s -> %s)".format(ink,p,i))
          val out = i - p
          po.put(ink,i)
          out
        }
        case other => {
          debug("resetting to zero: %s (%s)".format(ink,other))
          val out = 0.0
          po.put(ink,0.0)
          out
        }
      }
      (ink,updatedValue)
    }).toList:_*)).getOrElse(input)
    result
  }
  protected def interpretMuninData(tc:TelnetClient):Map[String,Map[String,Double]] = {
    val outputStream = new BufferedOutputStream(tc.getOutputStream)
    val inputStream = new BufferedInputStream(tc.getInputStream)
    var output = readStream(inputStream)
    if (output.length == 0)
      throw new DashboardException("Munin failed","no response from remote node")
    writeTo("list",outputStream)
    val possibleQueries = readStream(inputStream).split(" ").toList
    val desiredQueries = onlyFetch.filter(of => possibleQueries.contains(of.name))
    val completeOutput = Map(desiredQueries.map(of => {
      val o = of.name
      writeTo("fetch "+o,outputStream)
      val individualOutput = readStream(inputStream,commandResponseTerminator)
      val formattedOutput = Map(individualOutput.split("\n").filter(l => l.length > 1).map(l => {
        val parts = l.split(" ")
        val muninDataKey = parts(0).reverse.dropWhile(c => c != '.').drop(1).reverse.toString
        val muninDataValue = tryo(parts(1).toDouble).openOr(-1.0)
        (muninDataKey,muninDataValue)
      }):_*)
      val finalOutput = of.fieldType match {
        case Counter => {
          try {
            val deltas = generatedDelta(o, formattedOutput)
            Map(deltas.toList.map(fo => (fo._1,fo._2.toDouble)):_*)
          } catch {
            case _: Throwable => formattedOutput
          }
        }
        case Guage => {
          formattedOutput
        }
        case PercentageCounter => {
          try {
            val deltas = generatedDelta(o,formattedOutput)
            val total = deltas.values.sum
            val deltaPercentages = Map(deltas.toList.map(d => (d._1, ((d._2 / total) * 100))):_*)
            deltaPercentages
          } catch {
            case _: Throwable => formattedOutput
          }
        }
        case PercentageGuage => {
          try {
            val total = formattedOutput.values.sum
            Map(formattedOutput.toList.map(fo => (fo._1,((fo._2 / total) * 100))):_*)
          } catch {
            case _: Throwable => formattedOutput
          }
        }
      }
      (o,finalOutput)
    }):_*)
    writeTo("quit",outputStream)
    outputStream.close
    inputStream.close
    completeOutput
  }
  override def telnetBehaviour(tc:TelnetClient):List[String] = {
    val completeOutput = interpretMuninData(tc)
    //inform(true,previous.toMap)
    completeOutput.keys.map(cok => "%s -> %s".format(cok,completeOutput(cok))).toList
  }
}

case class MuninSensorAgainstThreshhold(metadata:SensorMetaData, host:String, port:Int, thresholds:Map[String,MuninCategoryDefinition] = Map.empty[String,MuninCategoryDefinition], time:TimeSpan = 5 seconds) extends MuninSensor(metadata,host,port,thresholds.values.toList,time){
  override def telnetBehaviour(tc:TelnetClient):List[String] = {
    val completeOutput = interpretMuninData(tc)
    var errors = List.empty[String]
    def addError(newError:String):Unit = {
      errors = errors ::: List(newError)
    }
    thresholds.keys.map(thresholdsKey => {
      val muninCatDef = thresholds(thresholdsKey)
      muninCatDef.matchers.keys.foreach(matcherKey => {
        completeOutput.get(thresholdsKey).map(completeOutputValueMap => {
          completeOutputValueMap.get(matcherKey).map(completeOutputValue => {
            val thresholdVerifier = muninCatDef.matchers(matcherKey)
            if (!thresholdVerifier.verify(completeOutputValue)){
              addError("%s.%s (%s) (%s)".format(thresholdsKey,matcherKey,completeOutputValue,thresholdVerifier.describe))
              false
            } else {
              true
            }
          }).getOrElse({
            addError("munin didn't return %s.%s".format(thresholdsKey,matcherKey))
            false
          })
        }).getOrElse({
          addError("munin didn't return %s".format(thresholdsKey))
          false
        })
      })
    })
    if (errors.length > 0){
      //inform(false,previous.toMap)
      throw new DashboardException("Munin failed to meet a specified threshold",errors.mkString("\r\n"))
    }
    //inform(true,previous.toMap)
    completeOutput.keys.map(cok => "%s -> %s".format(cok,completeOutput(cok))).toList
  }
}

