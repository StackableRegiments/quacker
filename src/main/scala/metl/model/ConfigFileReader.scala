package metl.model

import net.liftweb.common._
import scala.xml._

trait ConfigFileReader extends Logger {
	protected def getNodes(e:Node,tagName:String):List[Node] = (e \\ tagName).theSeq.toList
	protected def getImmediateNodes(e:Node,tagName:String):List[Node] = (e \ tagName).theSeq.toList
	protected def getChildrenNodes(e:Node):List[Node] = e.child.toList
	protected def getNodeString(e:Node,tagName:String):Option[String] = try {
		Some(getNodes(e,tagName).map(_.toString).mkString(""))
	} catch {
		case e:Throwable => None
	}
	protected def getAttr(e:Node,attrName:String):Option[String] = (e \ "@%s".format(attrName)).text.toString match {
		case t:String if t.length > 0 => Some(t)
		case _ => None.asInstanceOf[Option[String]]
	}
	protected def getText(e:Node,tag:String):Option[String] = (e \ tag).text.toString match {
		case t:String if t.length > 0 => Some(t)
		case _ => None.asInstanceOf[Option[String]]
	}
	protected def getBool(e:Node,tag:String):Option[Boolean] = getText(e,tag) match {
		case Some(t) if t.isInstanceOf[String] && t.toLowerCase.trim == "true" => Some(true)
		case Some(t) if t.isInstanceOf[String] && t.toLowerCase.trim == "false" => Some(false)
		case _ => None
	}
	protected def getInt(e:Node,tag:String):Option[Int] = getText(e,tag) match {
		case Some(t) if t.isInstanceOf[String] && t.length > 0 => {
			try {
				Some(t.toInt)
			} catch {
				case e:Throwable => None
			}
		}
		case _ => None
	}
	protected def getDouble(e:Node,tag:String):Option[Double] = getText(e,tag) match {
		case Some(t) if t.isInstanceOf[String] && t.length > 0 => {
			try {
				Some(t.toDouble)
			} catch {
				case e:Throwable => None
			}
		}
		case _ => None
	}
	protected def getLong(e:Node,tag:String):Option[Long] = getText(e,tag) match {
		case Some(t) if t.isInstanceOf[String] && t.length > 0 => {
			try {
				Some(t.toLong)
			} catch {
				case e:Throwable => None
			}
		}
		case _ => None
	}
}
