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

object ServiceConfigurator extends Logger {
  private val osName = System.getProperty("os.name")
  val isWindows = osName.toLowerCase.trim.startsWith("windows")
  val isLinux = osName.toLowerCase.trim.startsWith("linux")
  val isOSX = osName.toLowerCase.trim.startsWith("macos")
  private def getParamOrElse(paramName: String, orElse: => String): String = {
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
  def processDirectory(path: File, visit: File => Tuple2[Boolean, String])
    : Map[String, Tuple2[Boolean, String]] = {
    var items = Map.empty[String, Tuple2[Boolean, String]]
    path.listFiles
      .sortWith(_.getName < _.getName)
      .foreach(f => {
        val pathName = f.getAbsolutePath
        if (f.getName.endsWith(".disabled") || f.getName.contains("..")) {
          // skipping items
        } else if (f.isDirectory) {
          if (f.getName == ".git" || f.getName == ".svn" || f.getName == ".hg" || f.getName == "_svn" || f.getName == "_git" || f.getName == "_hg") {
            items = items.updated(
              pathName,
              (false,
               "ignoring version-control data directory: %s".format(pathName)))
          } else if (f.getName.endsWith(".disabled")) {
            items =
              items.updated(pathName,
                            (false, "skipping directory: %s".format(pathName)))
          } else {
            processDirectory(f, visit).toList.foreach(pdi => {
              items = items.updated(pdi._1, pdi._2)
            })
          }
        } else {
          items = items.updated(pathName, visit(f))
        }
      })
    items
  }
  def loadServices: Map[String, Tuple2[Boolean, String]] = {
    var errorThrown = false
    Globals.clearValidUsers
    Servers.clear
    ErrorRecorder.clear
    HistoryServer.clear
    println("loading file: %s".format(Globals.configDirectoryLocation))
    val output = processDirectory(
      new File(Globals.configDirectoryLocation),
      (f: File) => {
        if (f.getName.endsWith(".xml")) {
          loadServicesFromPath(f.getAbsolutePath)
        } else {
          (false, "did not load file".format(f.getAbsolutePath))
        }
      }
    )
    Servers.rebuildChecks
    output
  }
  def autoConfigure: Map[String, Tuple2[Boolean, String]] = {
    loadServices
  }
  def describeAutoConfigure(
      input: Map[String, Tuple2[Boolean, String]]): String = {
    input.toList
      .map(tuple => {
        val pathName = tuple._1
        val result = tuple._2
        val success = result._1
        val message = result._2
        "%s : %s%s".format(pathName, success, message match {
          case ""    => " "
          case other => " (%s)".format(other)
        })
      })
      .mkString("\r\n")
  }
  def loadServicesFromPath(xmlPath: String): Tuple2[Boolean, String] = {
    try {
      val x = scala.xml.XML.loadFile(xmlPath)
      var messages = List.empty[String]
      def safelyConfigure(name: String, usage: () => List[String]): Unit = {
        try {
          val outputMessages = usage()
          messages = messages ::: outputMessages
        } catch {
          case e: Throwable => {
            messages = messages ::: List(
              "%s failed to load xml with exception: %s".format(name,
                                                                e.getMessage))
          }
        }
      }
      safelyConfigure("validUsers", () => ValidUsers.configureFromXml(x))
      debug("In dev mode? %s".format(Globals.isDevMode))
      if (!Globals.isDevMode) {
        safelyConfigure("notifiers", () => ErrorRecorder.configureFromXml(x))
      }
      safelyConfigure("historyListeners",
                      () => HistoryServer.configureFromXml(x))
      safelyConfigure("servers", () => Servers.configureFromXml(x))
      (true, messages match {
        case l: List[String] if l.length > 0 => " %s".format(l.mkString(", "))
        case _                               => "loaded nothing"
      })
    } catch {
      case e: Throwable => {
        (false,
         "failed to load %s with exception: %s -> %s".format(xmlPath,
                                                             e,
                                                             e.getMessage))
      }
    }
  }
}
