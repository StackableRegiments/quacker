package com.metl.utils

import java.util.concurrent.{Future,TimeUnit}

import org.apache.hc.client5.http.async.methods.{ SimpleHttpRequest, SimpleHttpResponse, SimpleRequestBuilder,	SimpleRequestProducer, SimpleResponseConsumer }
import org.apache.hc.client5.http.impl.async.{ CloseableHttpAsyncClient, HttpAsyncClients }
import org.apache.hc.client5.http.config.{RequestConfig}
import org.apache.hc.core5.concurrent.FutureCallback
import org.apache.hc.core5.http.{HttpHost,Header,ContentType}
import org.apache.hc.core5.http.message.{StatusLine,BasicHeader}
import org.apache.hc.core5.io.CloseMode
import org.apache.hc.core5.reactor.IOReactorConfig
import org.apache.hc.core5.util.Timeout
import org.apache.hc.core5.http2.HttpVersionPolicy

import java.net.URI
import org.apache.commons.io.IOUtils

import java.util.Date
import net.liftweb.common.Logger
import net.liftweb.util.Helpers._

import org.apache.http.conn.ManagedClientConnection

trait IMeTLAsyncHttpClient {
	def start:Unit = {}
	def stop:Unit = {}
  def addAuthorization(domain: String, username: String, password: String): Unit

  def get(uri: String,callback:String=>Unit): Unit = get(uri, List.empty[(String, String)],callback)
  def get(uri: String, additionalHeaders: List[(String, String)],callback:String=>Unit): Unit
  def getExpectingHTTPResponse(
      uri: String,
      additionalHeaders: List[(String, String)] = List.empty[(String, String)],
      retriesSoFar: Int = 0,
      redirectsSoFar: Int = 0,
      exceptions: List[Throwable] = List.empty[Throwable],
      startTime: Long = new Date().getTime,
			callback: HTTPResponse => Unit):Unit

  def getAsString(uri: String,callback:String => Unit):Unit =
    getAsString(uri, List.empty[(String, String)],callback)
  def getAsString(uri: String,
                  additionalHeaders: List[(String, String)],callback:String => Unit): Unit

  def getAsBytes(uri: String,callback:Array[Byte]=>Unit):Unit =
    getAsBytes(uri, List.empty[(String, String)],callback)
  def getAsBytes(uri: String,
                 additionalHeaders: List[(String, String)],callback: Array[Byte]=>Unit):Unit

  def postBytes(uri: String, bytes: Array[Byte],callback:Array[Byte]=>Unit): Unit =
    postBytes(uri, bytes, List.empty[(String, String)],callback)
  def postBytes(uri: String,
                bytes: Array[Byte],
                additionalHeaders: List[(String, String)],callback: Array[Byte]=>Unit):Unit
  def postBytesExpectingHTTPResponse(
      uri: String,
      bytes: Array[Byte],
      additionalHeaders: List[(String, String)],callback: HTTPResponse=>Unit):Unit

  def postForm(uri: String, postItemList: List[(String, String)],callback: Array[Byte]=>Unit):Unit =
    postForm(uri, postItemList, List.empty[(String, String)],callback)
  def postForm(uri: String,
               postItemList: List[(String, String)],
               additionalHeaders: List[(String, String)], callback: Array[Byte]=>Unit):Unit
  def postFormExpectingHTTPResponse(
      uri: String,
      postItemList: List[(String, String)],
      additionalHeaders: List[(String, String)],callback: HTTPResponse=>Unit):Unit

  def postUnencodedForm(uri: String,
                        postItemList: List[(String, String)],callback: Array[Byte]=>Unit):Unit =
    postUnencodedForm(uri, postItemList, List.empty[(String, String)],callback)
  def postUnencodedForm(uri: String,
                        postItemList: List[(String, String)],
                        additionalHeaders: List[(String, String)],callback: Array[Byte]=>Unit):Unit
  def postUnencodedFormExpectingHttpResponse(
      uri: String,
      postItemList: List[(String, String)],
      additionalHeaders: List[(String, String)],callback: HTTPResponse=>Unit):Unit

  def setCookies(cookies: Map[String, Header]): Unit
  def getCookies: Map[String, Header]

  def setHttpHeaders(headers: List[Header]): Unit
  def getHttpHeaders: List[Header]
}

import org.apache.hc.client5.http.impl.nio.PoolingAsyncClientConnectionManagerBuilder
import org.apache.hc.client5.http.ssl.ClientTlsStrategyBuilder
import org.apache.hc.core5.http.ssl.TLS
import org.apache.hc.core5.pool.PoolConcurrencyPolicy
import org.apache.hc.core5.pool.PoolReusePolicy

object AsyncHelpers {
	lazy val trustingSslContext = org.apache.hc.core5.ssl.SSLContexts.custom().loadTrustMaterial(new org.apache.hc.client5.http.ssl.TrustAllStrategy).build()
	lazy val verifyingSslContext = org.apache.hc.core5.ssl.SSLContexts.createSystemDefault()
  protected class TrustAllHosts extends javax.net.ssl.HostnameVerifier {
    override def verify(_host: String, _session: javax.net.ssl.SSLSession) = true
  }
	lazy val verifyingHostnameVerifier = javax.net.ssl.HttpsURLConnection.getDefaultHostnameVerifier
	lazy val trustingHostnameVerifier = new TrustAllHosts
}

class CleanAsyncHttpClient(checkCerts:Boolean = false)
    extends IMeTLAsyncHttpClient
    with Logger {
  protected val connectionTimeout = 120
  //protected val connectionTimeout = 30
  protected val keepAliveTimeout = 120
  protected val readTimeout = 240000
  protected val maxRedirects = 20
  protected val maxRetries = 2
	protected val enableHttp2 = false


	protected val connMgr = {
		PoolingAsyncClientConnectionManagerBuilder.create()
		.setTlsStrategy(ClientTlsStrategyBuilder.create()
			.setHostnameVerifier(checkCerts match {
				case false => AsyncHelpers.trustingHostnameVerifier
				case true => AsyncHelpers.verifyingHostnameVerifier
			})
			.setSslContext(checkCerts match {
				case false => AsyncHelpers.trustingSslContext
				case true => AsyncHelpers.verifyingSslContext
			})
			.setTlsVersions(TLS.V_1_3, TLS.V_1_2)
			.build())
		.setPoolConcurrencyPolicy(PoolConcurrencyPolicy.STRICT)
		.setConnPoolPolicy(PoolReusePolicy.LIFO)
		.setConnectionTimeToLive(org.apache.hc.core5.util.TimeValue.ofMinutes(1L))
		.build();
	}
	protected val client = {
		HttpAsyncClients.custom()
		.setConnectionManager(connMgr)
		.setIOReactorConfig(IOReactorConfig.custom()
			.setSoTimeout(Timeout.ofSeconds(connectionTimeout))
			.build()
		)
		.evictExpiredConnections()
		.setVersionPolicy(enableHttp2 match {
			case false => HttpVersionPolicy.FORCE_HTTP_1
			case true => HttpVersionPolicy.NEGOTIATE
		})
		.evictIdleConnections(org.apache.hc.core5.util.TimeValue.ofMinutes(5L))
		.build()
	}
	override def start:Unit = {
		client.start()
	}
	override def stop:Unit = {
		client.close(CloseMode.IMMEDIATE)
	}
  private var authorizations: Map[String, (String, String)] = Map
    .empty[String, (String, String)]
    .withDefault((location) => ("anonymous", "unauthorized"))
  protected var cookies = Map.empty[String, Header]
  protected var httpHeaders = {
    Array[Header]()
  }
	protected val noAction = (mcc:ManagedClientConnection,s:String,s2:String) => {}
	protected val reqConfig = RequestConfig.custom()
		.setConnectTimeout(connectionTimeout,TimeUnit.SECONDS)
		.setDefaultKeepAlive(keepAliveTimeout,TimeUnit.SECONDS)
		.setHardCancellationEnabled(true)
		.setMaxRedirects(maxRedirects)
		.setRedirectsEnabled(true)
		.setResponseTimeout(readTimeout,TimeUnit.MILLISECONDS)
		.setCircularRedirectsAllowed(true)
		.build()
  override def setCookies(cook: Map[String, Header]): Unit = cookies = cook
  override def getCookies: Map[String, Header] = cookies

  def addHttpHeader(name: String, value: String): Unit =
    setHttpHeaders(getHttpHeaders ::: List(new BasicHeader(name, value)))
  override def setHttpHeaders(headers: List[Header]): Unit =
    httpHeaders = headers.toArray
  override def getHttpHeaders: List[Header] = httpHeaders.toList

  override def addAuthorization(domain: String,
                                username: String,
                                password: String): Unit = {
    authorizations = authorizations.updated(domain, (username, password))
  }
	
	protected def executeHttpCall(
		method:String,
		uri:String,
		headers:List[Tuple2[String,String]],
		body:Option[Either[Array[Byte],List[Tuple2[String,String]]]],
		callback:HTTPResponse=>Unit):Unit = {
		doExecuteHttpCall(method,uri,headers,body,callback,0,0)
	}
	protected def defaultHeaders(uri:String):List[Tuple2[String,String]] = {
		val u = new URI(uri)
		val port = u.getPort
		val host = u.getHost
		val blacklist = List(80,443,-1)
		List(
			("Host",blacklist.contains(port) match {
				case true => host
				case false => "%s:%s".format(host,port.toString)
			})
		)
	}
	protected def doExecuteHttpCall(
		method:String,
		uri:String,
		headers:List[Tuple2[String,String]],
		body:Option[Either[Array[Byte],List[Tuple2[String,String]]]],
		callback:HTTPResponse=>Unit,
		retryNumber:Int = 0,
		redirectNumber:Int = 0,
		exceptionsSoFar: List[Throwable] = List.empty[Throwable],
		start: Long = new Date().getTime
	):Unit = {
		try {
			if ((maxRedirects > 0) && (redirectNumber > maxRedirects || exceptionsSoFar
						.filter(e => e.isInstanceOf[RedirectException])
						.length > maxRedirects)) {
				throw new RedirectException(
					"exceeded configured maximum number of redirects (%s) when requesting: %s"
						.format(maxRedirects, uri),
					exceptionsSoFar)
			}
			if ((maxRetries > 0) && (retryNumber > maxRetries)) {
				throw new RetryException(
					"exceed maximum number of retries (%s) when requesting: %s"
						.format(maxRetries, uri),
					exceptionsSoFar)
			}
			val initReq = (method.trim.toLowerCase match {
				case "get" => SimpleRequestBuilder.get()
				case "post" => SimpleRequestBuilder.post()
				case "put" => SimpleRequestBuilder.put()
				case "patch" => SimpleRequestBuilder.patch()
				case "delete" => SimpleRequestBuilder.delete()
				case "head" => SimpleRequestBuilder.head()
				case "trace" => SimpleRequestBuilder.trace()
				case "options" => SimpleRequestBuilder.options()
			}).setUri(uri).setHeaders((getHttpHeaders ::: ((defaultHeaders(uri) ::: headers).map(h => {
				new BasicHeader(h._1,h._2)
			}))):_*).setRequestConfig(reqConfig)
			val req = body.map{
				case Left(bytes) => {
					val contentType = headers.find(_._1.toLowerCase.trim == "content-type").map(h => ContentType.parse(h._2)).getOrElse(ContentType.APPLICATION_OCTET_STREAM)
					initReq.setBody(bytes,contentType)/*.setHeader("Content-Type",contentType.toString).setHeader("Content-Length",bytes.length.toString)*/
				}
				case Right(formItems) => {
					val contentType = ContentType.parse("application/x-www-form-urlencoded") //ContentType.APPLICATION_FORM_URLENCODED
					val form = formItems.map(fi => {
						"%s=%s".format(urlEncode(fi._1),urlEncode(fi._2))
					}).mkString("&")
					initReq.setBody(form,contentType)/*.setHeader("Content-Type",contentType.toString).setHeader("Content-Length",form.length.toString)*/
				}
			}.getOrElse(initReq)
			val request = req.build()
			val futureResp = client.execute(
				SimpleRequestProducer.create(request),
				SimpleResponseConsumer.create(),
				new FutureCallback[SimpleHttpResponse](){
					override def completed(resp:SimpleHttpResponse) = {
						val hResp = HTTPResponse(
							uri,
							noAction,
							resp.getBodyBytes(),
							resp.getCode(),
							Map(resp.getHeaders().toList.map(h => {
								Tuple2(h.getName,h.getValue)
							}):_*),
							start,
							new Date().getTime()
						)
						callback(hResp)
					}
					override def failed(ex:Exception) = {
						throw ex
					}
					override def cancelled() = {
						throw new Exception("cancelled")
					}
				}
			)
			futureResp.get
		} catch {
			case ex: RetryException =>
				throw new RetryException(ex.getMessage, ex.exceptions)
			case ex: RedirectException =>
				throw new RedirectException(ex.getMessage, ex.exceptions)
			case ex: Throwable => throw ex
		}
	}

  override def postBytes(uri: String,
                         bytes: Array[Byte],
                         additionalHeaders: List[(String, String)] =
                           List.empty[(String, String)], callback:Array[Byte] => Unit): Unit = {
		postBytesExpectingHTTPResponse(uri, bytes, additionalHeaders,(hr:HTTPResponse) => {
			respondToResponse(hr,additionalHeaders,(hr2:HTTPResponse) => {
				callback(hr2.bytes)
			})
		})
	}
  override def postBytesExpectingHTTPResponse(
      uri: String,
      bytes: Array[Byte],
      additionalHeaders: List[(String, String)] = List.empty[(String, String)],
			callback:HTTPResponse=>Unit)
    : Unit = {
		executeHttpCall("post",uri, additionalHeaders, Some(Left(bytes)), callback)
	}
  override def postForm(uri: String,
                        postItemList: List[(String, String)],
                        additionalHeaders: List[(String, String)] =
                          List.empty[(String, String)],
												callback: Array[Byte]=>Unit):Unit = {
		postFormExpectingHTTPResponse(uri, postItemList, additionalHeaders,(hr:HTTPResponse) => {
			respondToResponse(hr,additionalHeaders,(hr2:HTTPResponse) => {
				callback(hr2.bytes)
			})
		})
	}
  override def postFormExpectingHTTPResponse(
      uri: String,
      postItemList: List[(String, String)],
      additionalHeaders: List[(String, String)] = List.empty[(String, String)],
			callback:HTTPResponse=>Unit):Unit = {
		executeHttpCall("post", uri, additionalHeaders, Some(Right(postItemList)), callback)
	}
  override def postUnencodedForm(uri: String,
                                 postItemList: List[(String, String)],
                                 additionalHeaders: List[(String, String)] =
                                   List.empty[(String, String)],
																	callback: Array[Byte]=>Unit):Unit = {
        postUnencodedFormExpectingHttpResponse(uri, postItemList, additionalHeaders,(hr:HTTPResponse) => {
					respondToResponse(hr,additionalHeaders,(hr2:HTTPResponse) => {
						callback(hr2.bytes)
					})
				})
	}
  override def postUnencodedFormExpectingHttpResponse(
      uri: String,
      postItemList: List[(String, String)],
      additionalHeaders: List[(String, String)] = List.empty[(String, String)],
			callback:HTTPResponse=>Unit)
    : Unit = {
		val postForm = postItemList
			.map(postItem => postItem._1 + "=" + postItem._2)
			.mkString("&")
		val bytes = postForm.getBytes("UTF-8")
		executeHttpCall("post",uri, additionalHeaders ::: List(
			Tuple2("Content-Type", """application/x-www-form-urlencoded"""),
			Tuple2("Content-Length",bytes.length.toString)
		),Some(Left(bytes)), callback)
	}
  override def get(uri: String,
                   additionalHeaders: List[(String, String)] =
                     List.empty[(String, String)],
									callback:String=>Unit): Unit = {
      getAsString(uri, additionalHeaders,callback)
	}
  override def getAsString(uri: String,
                           additionalHeaders: List[(String, String)] =
                             List.empty[(String, String)],
													callback:String => Unit): Unit = {
		getAsBytes(uri,additionalHeaders,(hr:Array[Byte]) => {
			callback(IOUtils.toString(hr))
		})
	}
  override def getAsBytes(uri: String,
                          additionalHeaders: List[(String, String)] =
                            List.empty[(String, String)],callback: Array[Byte]=>Unit):Unit = {
		getExpectingHTTPResponse(uri, additionalHeaders,0,0,Nil,new Date().getTime(),(hr:HTTPResponse) => {
      respondToResponse(hr,additionalHeaders,(hr2:HTTPResponse) => {
				callback(hr2.bytes)
			})
		})
	}
  override def getExpectingHTTPResponse(
      uri: String,
      additionalHeaders: List[(String, String)] = List.empty[(String, String)],
      retriesSoFar: Int = 0,
      redirectsSoFar: Int = 0,
      exceptions: List[Throwable] = List.empty[Throwable],
      startTime: Long = new Date().getTime,
			callback:HTTPResponse => Unit): Unit = {
			executeHttpCall("get",uri,additionalHeaders,None,callback)
	}
  def respondToResponse(response: HTTPResponse,
                        additionalHeaders: List[(String, String)] = List.empty[(String, String)],
												callback:HTTPResponse=>Unit):Unit = {
    val uri = response.requestUrl
    val tempOutput = response.bytes
    response.statusCode match {
      case 200 => callback(response)
/*
      case 300 | 301 | 302 | 303 => {
        val newLoc = response.headers("Location")
        val newLocUri = new URI(newLoc)
        val oldLoc = new URI(uri)
        val newLocString = if (newLocUri.getHost == null) {
          oldLoc.resolve(newLocUri).toString
        } else {
          newLoc
        }
          getExpectingHTTPResponse(
            newLocString,
            additionalHeaders,
            response.numberOfRetries,
            response.numberOfRedirects + 1,
            response.exceptions ::: List(
              new RedirectException(
                "healthy redirect from %s to %s".format(uri, newLocString),
                response.exceptions)),
            response.startMilis,
						callback
          )
      }
      case 307 => {
        val newLoc = response.headers("Location")
        val newLocUri = new URI(newLoc)
        val oldLoc = new URI(uri)
        val newLocString = if (newLocUri.getHost == null) {
          oldLoc.resolve(newLocUri).toString
        } else {
          newLoc
        }
          getExpectingHTTPResponse(
            newLocString,
            additionalHeaders,
            response.numberOfRetries,
            response.numberOfRedirects + 1,
            response.exceptions ::: List(
              new RedirectException(
                "healthy redirect from %s to %s".format(uri, newLocString),
                response.exceptions)),
            response.startMilis,
						callback
          )
      }
*/
      /*
      case 400 =>
        throw new WebException(
          "bad request sent to %s: %s".format(uri, tempOutput),
          400,
          uri)
      case 401 =>
        throw new WebException(
          "access to object at %s requires authentication".format(uri),
          401,
          uri)
      case 403 =>
        throw new WebException("access forbidden to object at %s".format(uri),
                               403,
                               uri)
      case 404 =>
        throw new WebException("object not found at %s".format(uri), 404, uri)
      case 500 =>
        throw new WebException("server error encountered at %s: %s"
                                 .format(uri, response.responseAsString),
                               500,
                               uri)
      case other =>
        throw new WebException(
          "http status code (%s) not yet implemented, returned from %s"
            .format(other, uri),
          other,
          uri)
       */
      case other => callback(response)
    }
  }
}

object AsyncHttp {
  def getClient = Stopwatch.time("AsyncHttp.getClient", {
		new CleanAsyncHttpClient()
	})
  def getAuthedClient(username: String,
                      password: String,
                      domain: String = "*") = {
    Stopwatch.time("AsyncHttp.getAuthedClient", {
      val client = new CleanAsyncHttpClient()
      client.addAuthorization(domain, username, password)
      client
    })
	}
  def cloneClient(incoming: CleanAsyncHttpClient): CleanAsyncHttpClient = {
    Stopwatch.time( "AsyncHttp.cloneClient", {
			val client = new CleanAsyncHttpClient()
			client.setCookies(incoming.getCookies)
			client.setHttpHeaders(incoming.getHttpHeaders)
			client
		})
	}
  def getClient(headers: List[(String, String)]): CleanAsyncHttpClient = {
    Stopwatch.time( "AsyncHttp.getClient(headers)", {
			val newHeaders = headers.map(tup => new BasicHeader(tup._1, tup._2)).toList
			val client = new CleanAsyncHttpClient() {
				override val connectionTimeout = 3600
				override val keepAliveTimeout = 5400
				override val readTimeout = 7200000
			}
			client.setHttpHeaders(newHeaders)
			client
		})
	}
}
