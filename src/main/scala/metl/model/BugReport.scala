package metl.model

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import xml._
import java.util.Date
import net.liftweb.http.provider.HTTPCookie
//email
import net.liftweb.util.Mailer
import net.liftweb.util.Mailer._

object BugReport {

	private val successTemplate = Templates(List("_bugReportSuccess")).openOr(NodeSeq.Empty)
	private val failureTemplate = Templates(List("_bugReportFailure")).openOr(NodeSeq.Empty)
	private def constructNode(in:NodeSeq):Node = {
		//S.skipXmlHeader_=(true)
		in.theSeq.headOption.getOrElse(Text("failed to construct response message")) 
	}
	private def successResponse(message:String) = XhtmlResponse(constructNode(("#message" #> Text(message)).apply(successTemplate)), Full(DocType.xhtmlTransitional), List.empty[Tuple2[String,String]], List.empty[HTTPCookie], 200, true)
	private def failResponse(message:String) = XhtmlResponse(constructNode(("#message" #> Text(message)).apply(failureTemplate)), Full(DocType.xhtmlTransitional), List.empty[Tuple2[String,String]], List.empty[HTTPCookie], 500, true)

	private val unknownName = "[bug not named]"
	private val unknownUser = "[username not supplied]"
	private val unknownEmail = "[email not supplied]"
	private val unknownLocation = "[location not supplied]"
	private val unknownTime = "[time not supplied]"
	private val unknownDescription = "[description not supplied]"

	private def emailBody(user:String,email:String,name:String,time:String,location:String,description:String) = {
		 """%s (email: %s) reported the following bug at %s:
		Bug Name: %s
		When: %s
		Where: %s
		Bug Description: %s
		""".format(user, email, new Date().toString, name, time, location, description)
	}

	private def getParam(incomingRequest:Req,paramName:String,default:String) = {
		incomingRequest.param(paramName) match {
			case Full(result) if (result.length == 0) => default
			case Full(result) => result
			case _ => default
		}
	}

	def handleBug(req:Req):LiftResponse = {
		tryo({
			val name = getParam(req,"summary",unknownName) 
			val reportingUser = getParam(req,"userName",unknownUser)
			val email = getParam(req,"emailAddress",unknownEmail) 
			val location = getParam(req,"locationOccurred",unknownLocation)
			val time = getParam(req,"timeOccurred",unknownTime)
			val description = getParam(req,"bugDescription",unknownDescription)
			if (name != unknownName || description != unknownDescription){ 
				val message =	emailBody(reportingUser,email,name,time,location,description)
				Mailer.sendMail(From("bug.reporter@stackableregiments.com"),Subject("bug report"),PlainMailBodyType(message) :: List(To("bug.acceptort@stackableregiments.com")):_*)
				successResponse("bug reported successfully")
			}
			else failResponse("Please supply either a summary or a description")
		}).openOr(failResponse("Error while submitting bug"))
	}
}
