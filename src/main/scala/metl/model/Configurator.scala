package metl.model

import metl.comet._
import scala.xml._
import net.liftweb._
import common._
import util._
import Helpers._

import javax.mail._
import javax.mail.internet._
import java.io.File

trait JsonReader {
  import net.liftweb.json._
  import Serialization._
  protected implicit val formats: Formats = DefaultFormats
  protected def asArray(jv: JValue): List[JValue] = {
    jv.extractOpt[JArray].toList.flatMap(_.children)
  }
  protected def asArrayOfObjs(jv: JValue): List[JObject] = {
    asArray(jv).flatMap {
      case jo: JObject => Some(jo)
      case _           => None
    }
  }
  protected def asArrayOfStrings(jv: JValue): List[String] = {
    asArray(jv).flatMap {
      case JString(s) => Some(s)
      case _          => None
    }
  }
  protected def asString(jv: JValue): Option[String] = {
    jv.extractOpt[String]
  }
  protected def optOnKey[A](jv: JValue,
                            key: String,
                            valueFunc: JValue => A): Option[A] = {
    (jv \ key).extractOpt[JValue].map(valueFunc)
  }
  protected def hasKey(jv: JValue, key: String): Boolean =
    (jv \ key).extractOpt[JValue].isDefined
}

object ServiceConfigurator extends Logger {
  private val osName = System.getProperty("os.name")
  val isWindows = osName.toLowerCase.trim.startsWith("windows")
  val isLinux = osName.toLowerCase.trim.startsWith("linux")
  val isOSX = osName.toLowerCase.trim.startsWith("macos")
  def getParamOrElse(paramName: String, orElse: => String): String = {
    System.getProperty(paramName) match {
      case s: String if (s.length > 0) => s
      case _ => {
        Props.get(paramName) match {
          case Full(s) => s
          case _       => orElse
        }
      }
    }
  }
}
