package metl.model.sensor

import com.metl.utils.{HTTPResponse, Http, AsyncHttp}
import metl.model._
import net.liftweb.common.Full
import net.liftweb.util.Helpers._

import scala.xml.Node

object HTTPResponseMatchers extends ConfigFileReader {
  def configureFromXml(n: Node): HTTPResponseMatcher = {
    val matcher = new HTTPResponseMatcher
    getNodes(n, "matcher").map(mn => {
      getAttr(mn, "name").getOrElse("unknown") match {
        case "response" =>
          matcher.setResponseVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "requestUrl" =>
          matcher.setRequestUrlVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "duration" =>
          matcher.setDurationVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "statusCode" =>
          matcher.setCodeVerifier(Matchers.configureVerificationFuncFromXml(mn))
        case "headers" =>
          matcher.setHeadersVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "retries" =>
          matcher.setRetriesVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "redirects" =>
          matcher.setRedirectsVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "exceptions" =>
          matcher.setExceptionsVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case _ => {}
      }
    })
    matcher
  }
  object EmptyHTTPResponseMatcher extends HTTPResponseMatcher {
    override def setResponseVerifier(matcher: Matcher): Unit = {}
    override def setRequestUrlVerifier(matcher: Matcher): Unit = {}
    override def setDurationVerifier(matcher: Matcher): Unit = {}
    override def setCodeVerifier(matcher: Matcher): Unit = {}
    override def setHeadersVerifier(matcher: Matcher): Unit = {}
    override def setRetriesVerifier(matcher: Matcher): Unit = {}
    override def setRedirectsVerifier(matcher: Matcher): Unit = {}
    override def setExceptionsVerifier(matcher: Matcher): Unit = {}
    override def verify(response: HTTPResponse): VerificationResponse =
      VerificationResponse(true, List.empty[String])
  }
  val empty = EmptyHTTPResponseMatcher
  def default = new HTTPResponseMatcher
}

case class VerificationResponse(success: Boolean, errors: List[String])
class HTTPResponseMatcher {
  protected var responseVerifier: Option[Matcher] = None
  def setResponseVerifier(matcher: Matcher): Unit =
    responseVerifier = Some(matcher)
  protected var requestUrlVerifier: Option[Matcher] = None
  def setRequestUrlVerifier(matcher: Matcher): Unit =
    requestUrlVerifier = Some(matcher)
  protected var durationVerifier: Option[Matcher] = None
  def setDurationVerifier(matcher: Matcher): Unit =
    durationVerifier = Some(matcher)
  protected var codeVerifier: Option[Matcher] = Some(
    IsNumericallyEqualsMatcher(200))
  def setCodeVerifier(matcher: Matcher): Unit = codeVerifier = Some(matcher)
  protected var headersVerifier: Option[Matcher] = None
  def setHeadersVerifier(matcher: Matcher): Unit =
    headersVerifier = Some(matcher)
  protected var retriesVerifier: Option[Matcher] = None
  def setRetriesVerifier(matcher: Matcher): Unit =
    retriesVerifier = Some(matcher)
  protected var redirectsVerifier: Option[Matcher] = None
  def setRedirectsVerifier(matcher: Matcher): Unit =
    redirectsVerifier = Some(matcher)
  protected var exceptionsVerifier: Option[Matcher] = None
  def setExceptionsVerifier(matcher: Matcher): Unit =
    exceptionsVerifier = Some(matcher)
  def verify(response: HTTPResponse): VerificationResponse = {
    var errors = List.empty[String]
    responseVerifier.map(m => {
      val responseString = response.responseAsString
      if (!m.verify(responseString))
        errors = errors ::: List(
          "response (%s) failed verification %s".format(responseString,
                                                        m.describe))
    })
    requestUrlVerifier.map(m => {
      if (!m.verify(response.requestUrl))
        errors = errors ::: List(
          "requestUrl (%s) failed verification %s".format(response.requestUrl,
                                                          m.describe))
    })
    durationVerifier.map(m => {
      if (!m.verify(response.duration))
        errors = errors ::: List(
          "duration (%s) failed verification %s".format(response.duration,
                                                        m.describe))
    })
    codeVerifier.map(m => {
      if (!m.verify(response.statusCode))
        errors = errors ::: List(
          "statusCode (%s) failed verification %s".format(response.statusCode,
                                                          m.describe))
    })
    headersVerifier.map(m => {
      val headersString = response.headers.toList
        .map(hi => "%s : %s".format(hi._1, hi._2))
        .mkString("\r\n")
      if (!m.verify(headersString))
        errors = errors ::: List(
          "headers (%s) failed verification %s".format(headersString,
                                                       m.describe))
    })
    retriesVerifier.map(m => {
      if (!m.verify(response.numberOfRetries))
        errors = errors ::: List(
          "retries (%s) failed verification %s".format(response.numberOfRetries,
                                                       m.describe))
    })
    redirectsVerifier.map(m => {
      if (!m.verify(response.numberOfRedirects))
        errors = errors ::: List(
          "redirects (%s) failed verification %s"
            .format(response.numberOfRedirects, m.describe))
    })
    exceptionsVerifier.map(m => {
      val exceptionsString =
        response.exceptions.map(ex => ex.getMessage).mkString("\r\n")
      if (!m.verify(exceptionsString))
        errors = errors ::: List(
          "exceptions (%s) failed verification %s".format(exceptionsString,
                                                          m.describe))
    })
    VerificationResponse(errors.length == 0, errors)
  }
}

case class HttpSensor(
    metadata: SensorMetaData,
    uri: String,
    headers: List[Tuple2[String, String]] = List.empty[Tuple2[String, String]],
    matcher: HTTPResponseMatcher = HTTPResponseMatchers.default,
    time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  def getClient = AsyncHttp.getClient
  var client = getClient
  headers.foreach(h => client.addHttpHeader(h._1, h._2))
  override def resetEnvironment = {
		client.stop
    client = getClient
		client.start
    headers.foreach(h => client.addHttpHeader(h._1, h._2))
  }
  override def performCheck(after:() => Unit) = {
		val timeout = checkTimeout.toOption.map(_.millis)
		client.getExpectingHTTPResponse(uri,Nil,0,0,Nil,new java.util.Date().getTime(),(response1:HTTPResponse) => {
			client.respondToResponse(response1,Nil,(response:HTTPResponse) => {
				val verificationResponse = matcher.verify(response)
				if (!verificationResponse.success) {
					throw new DashboardException("HTTP Verification failed",
																			 verificationResponse.errors.mkString("\r\n"))
				}
				succeed(response.toString, Full(response.duration.toDouble))
				after()
			},timeout)
		},timeout)
	}
}
case class HttpSensorWithBasicAuth(
    metadata: SensorMetaData,
    uri: String,
    username: String,
    password: String,
    headers: List[Tuple2[String, String]] = List.empty[Tuple2[String, String]],
    matcher: HTTPResponseMatcher = HTTPResponseMatchers.default,
    time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  def getClient = AsyncHttp.getAuthedClient(username, password)
  var client = getClient
  headers.foreach(h => client.addHttpHeader(h._1, h._2))
  override def resetEnvironment = {
    client = getClient
    headers.foreach(h => client.addHttpHeader(h._1, h._2))
  }
  override def performCheck(after:() => Unit) = {
		val timeout = checkTimeout.toOption.map(_.millis)
		client.getExpectingHTTPResponse(uri,Nil,0,0,Nil,new java.util.Date().getTime(),(response1:HTTPResponse) => {
			client.respondToResponse(response1,Nil,(response:HTTPResponse) => {
				val verificationResponse = matcher.verify(response)
				if (!verificationResponse.success) {
					throw new DashboardException("HTTP Verification failed",
																			 verificationResponse.errors.mkString("\r\n"))
				}
				succeed(response.toString, Full(response.duration.toDouble))
				after()
			},timeout)
		},timeout)
	}
}

case class SyncHttpSensor(
    metadata: SensorMetaData,
    uri: String,
    headers: List[Tuple2[String, String]] = List.empty[Tuple2[String, String]],
    matcher: HTTPResponseMatcher = HTTPResponseMatchers.default,
    time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  def getClient = Http.getClient
  var client = getClient
  headers.foreach(h => client.addHttpHeader(h._1, h._2))
  override def resetEnvironment = {
		client.stop
    client = getClient
		client.start
    headers.foreach(h => client.addHttpHeader(h._1, h._2))
  }
  def status = {
    val response =
      client.respondToResponse(client.getExpectingHTTPResponse(uri))
    val verificationResponse = matcher.verify(response)
    if (!verificationResponse.success) {
      throw new DashboardException("HTTPwithAuth Verification failed",
                                   verificationResponse.errors.mkString("\r\n"))
    }
    (response, Full(response.duration.toDouble))
  }
  override def performCheck(after:() => Unit) = {
		val s = status
		succeed(s._1.toString, s._2)
		after()
	}
}
case class SyncHttpSensorWithBasicAuth(
    metadata: SensorMetaData,
    uri: String,
    username: String,
    password: String,
    headers: List[Tuple2[String, String]] = List.empty[Tuple2[String, String]],
    matcher: HTTPResponseMatcher = HTTPResponseMatchers.default,
    time: TimeSpan = 5 seconds)
    extends Sensor(metadata) {
  override val pollInterval = time
  def getClient = Http.getAuthedClient(username, password)
  var client = getClient
  headers.foreach(h => client.addHttpHeader(h._1, h._2))
  override def resetEnvironment = {
    client = getClient
    headers.foreach(h => client.addHttpHeader(h._1, h._2))
  }
  def status = {
    val response =
      client.respondToResponse(client.getExpectingHTTPResponse(uri))
    val verificationResponse = matcher.verify(response)
    if (!verificationResponse.success) {
      throw new DashboardException("HTTPwithAuth Verification failed",
                                   verificationResponse.errors.mkString("\r\n"))
    }
    (response, Full(response.duration.toDouble))
  }
  override def performCheck(after:() => Unit) = {
		val s = status
		succeed(s._1.toString, s._2)
		after()
	}
}
