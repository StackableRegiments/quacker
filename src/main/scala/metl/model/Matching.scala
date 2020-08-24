package metl.model

import metl.model.sensor.VerificationResponse

import scala.xml._
import scala.util.matching.Regex

class Matcher {
  def verify(input: Any): Boolean = false
  def describe: String = "no description supplied"
  def getSubMatchers: List[Matcher] = List.empty[Matcher]
}
case class DependencyCheckMatcher(description: DependencyDescription,
                                  condition: DependencyMatcher)
    extends Matcher {
  override def verify(input: Any): Boolean =
    condition.verify(description) match {
      case VerificationResponse(true, Nil)     => true
      case VerificationResponse(false, errors) => false
      case other                               => false
    }
  override def describe: String =
    "(%s%s%s %s)".format(
      description.service.map(s => "%s:".format(s)).getOrElse(""),
      description.service.map(s => "%s:".format(s)).getOrElse(""),
      description.pinger,
      condition.describe
    )
  override def getSubMatchers: List[Matcher] = List.empty[Matcher]

}
case class NoneMatcher(subMatchers: List[Matcher]) extends Matcher {
  override def verify(input: Any): Boolean =
    subMatchers.length > 0 && !(subMatchers.exists(st => st.verify(input)))
  override def describe: String =
    "(not %s)".format(subMatchers.map(st => st.describe).mkString(" and not "))
  override def getSubMatchers: List[Matcher] = subMatchers
}
case class SomeMatcher(subMatchers: List[Matcher] = List.empty[Matcher])
    extends Matcher {
  override def verify(input: Any): Boolean =
    subMatchers.length > 0 && subMatchers.exists(st => st.verify(input))
  override def describe: String =
    "(%s)".format(subMatchers.map(st => st.describe).mkString(" or "))
  override def getSubMatchers: List[Matcher] = subMatchers
}
case class SomeNotMatcher(subMatchers: List[Matcher] = List.empty[Matcher])
    extends Matcher {
  override def verify(input: Any): Boolean =
    subMatchers.length > 0 && subMatchers.exists(st => !st.verify(input))
  override def describe: String =
    "(not %s)".format(subMatchers.map(st => st.describe).mkString(" or not "))
  override def getSubMatchers: List[Matcher] = subMatchers
}
case class NotSomeMatcher(subMatchers: List[Matcher] = List.empty[Matcher])
    extends Matcher {
  override def verify(input: Any): Boolean =
    subMatchers.length > 0 && (!(subMatchers
      .exists(st => st.verify(input))) || !(subMatchers.exists(st =>
      !st.verify(input))))
  override def describe: String =
    "(not some of %s)".format(
      subMatchers.map(st => st.describe).mkString(" and "))
  override def getSubMatchers: List[Matcher] = subMatchers
}
case class AllMatcher(subMatchers: List[Matcher] = List.empty[Matcher])
    extends Matcher {
  override def verify(input: Any): Boolean =
    subMatchers.length > 0 && !(subMatchers.exists(st => !st.verify(input)))
  override def describe: String =
    "(%s)".format(subMatchers.map(st => st.describe).mkString(" and "))
  override def getSubMatchers: List[Matcher] = subMatchers
}
class SingleMatcher(name: String, trueIf: Any => Boolean) extends Matcher {
  override def verify(input: Any): Boolean = trueIf(input)
  override def describe: String = "(%s)".format(name)
}

case class IsLessThanMatcher(matcherValue: Double)
    extends SingleMatcher(
      "is less than %s".format(matcherValue),
      (in: Any) =>
        in match {
          case d: Double => { d < matcherValue }
          case i: Int    => { i < matcherValue.toInt }
          case lo: Long  => { lo < matcherValue.toLong }
          case f: Float  => { f < matcherValue.toFloat }
          case other     => false
      }
    )
case class IsGreaterThanMatcher(matcherValue: Double)
    extends SingleMatcher(
      "is greater than %s".format(matcherValue),
      (in: Any) =>
        in match {
          case d: Double => { d > matcherValue }
          case i: Int    => { i > matcherValue.toInt }
          case l: Long   => { l > matcherValue.toLong }
          case f: Float  => { f > matcherValue.toFloat }
          case other     => false
      }
    )
case class IsNumericallyEqualsMatcher(matcherValue: Double)
    extends SingleMatcher(
      "is numerically equal to %s".format(matcherValue),
      (in: Any) =>
        in match {
          case d: Double => matcherValue == d
          case i: Int    => matcherValue.toInt == i
          case l: Long   => matcherValue.toLong == l
          case f: Float  => matcherValue.toFloat == f
          case other     => false
      }
    )
class IsBooleanEqualsMatcher(matcherValue: Boolean)
    extends SingleMatcher("is boolean equal to %s".format(matcherValue),
                          (in: Any) =>
                            in match {
                              case b: Boolean => b == matcherValue
                              case _          => false
                          })
case object IsTrueMatcher extends IsBooleanEqualsMatcher(true) {
  override def describe = "is true"
}
case object IsNotTrueMatcher extends IsBooleanEqualsMatcher(false) {
  override def describe = "is false"
}
case class IsStringEqualsMatcher(matcherValue: String)
    extends SingleMatcher("has string equality with %s".format(matcherValue),
                          (in: Any) => in.toString == matcherValue)
case class IsStringEqualsIgnoreCaseMatcher(matcherValue: String)
    extends SingleMatcher(
      "has case-insensitive string equality with %s".format(matcherValue),
      (in: Any) => in.toString.toLowerCase.trim == matcherValue)
case class IsStringContainsMatcher(matcherValue: String)
    extends SingleMatcher("contains string %s".format(matcherValue),
                          (in: Any) => in.toString.contains(matcherValue))
case class IsStringContainsIgnoreCaseMatcher(matcherValue: String)
    extends SingleMatcher(
      "contains case-insensitive string %s".format(matcherValue),
      (in: Any) => in.toString.toLowerCase.trim.contains(matcherValue))
case class IsRegexMatchedMatcher(matcherValue: String, regexPattern: Regex)
    extends SingleMatcher(
      "matches regex %s".format(matcherValue),
      (in: Any) =>
        regexPattern.findFirstIn(in.toString).map(v => true).getOrElse(false))

case class DependencyDescription(pinger: String,
                                 server: Option[String],
                                 service: Option[String],
                                 serviceCheckMode: Option[ServiceCheckMode])
object DependencyMatchers extends ConfigFileReader {
  import net.liftweb.json._
  import Serialization._
  protected implicit val formats = DefaultFormats
  def configureFromJson(jv: JValue): DependencyMatcher = {
    val matcher = new DependencyMatcher
    //TODO
    matcher
  }

  def configureFromXml(n: Node): DependencyMatcher = {
    val matcher = new DependencyMatcher
    getNodes(n, "matcher").map(mn => {
      getAttr(mn, "name").getOrElse("unknown") match {
        case "lastCheckBegin" =>
          matcher.setLastCheckBeginVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "lastUptime" =>
          matcher.setLastUptimeVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "lastCheckCompleted" =>
          matcher.setLastCheckVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "lastWhy" =>
          matcher.setLastWhyVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "lastDetails" =>
          matcher.setLastDetailsVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "lastStatus" =>
          matcher.setLastStatusVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case "checkIsRunning" =>
          matcher.setIsRunningVerifier(
            Matchers.configureVerificationFuncFromXml(mn))
        case _ => {}
      }
    })
    matcher
  }
  def default = new DependencyMatcher
}
class DependencyMatcher {
  protected var lastCheckBeginVerifier: Option[Matcher] = None
  def setLastCheckBeginVerifier(m: Matcher) = lastCheckBeginVerifier = Some(m)
  protected var lastUptimeVerifier: Option[Matcher] = None
  def setLastUptimeVerifier(m: Matcher) = lastUptimeVerifier = Some(m)
  protected var lastCheckVerifier: Option[Matcher] = None
  def setLastCheckVerifier(m: Matcher) = lastCheckVerifier = Some(m)
  protected var lastWhyVerifier: Option[Matcher] = None
  def setLastWhyVerifier(m: Matcher) = lastWhyVerifier = Some(m)
  protected var lastDetailsVerifier: Option[Matcher] = None
  def setLastDetailsVerifier(m: Matcher) = lastDetailsVerifier = Some(m)
  protected var lastStatusVerifier: Option[Matcher] = Some(IsTrueMatcher)
  def setLastStatusVerifier(m: Matcher) = lastStatusVerifier = Some(m)
  protected var isRunningVerifier: Option[Matcher] = Some(IsTrueMatcher)
  def setIsRunningVerifier(m: Matcher) = isRunningVerifier = Some(m)
  def describe: String =
    List(
      ("last check start time", lastCheckBeginVerifier),
      ("last successful check time", lastUptimeVerifier),
      ("last check finish time", lastCheckVerifier),
      ("last check why", lastWhyVerifier),
      ("last check details", lastDetailsVerifier),
      ("last check status", lastStatusVerifier),
      ("last check isRunning", isRunningVerifier)
    ).filter(f => f._2 != None)
      .map(
        v =>
          "%s %s".format(
            v._1,
            v._2.map(m => m.describe).getOrElse("has unspecified condition")))
      .mkString(" and ")
  def verify(dependency: DependencyDescription): VerificationResponse = {
    var errors = List.empty[String]
    val pingersToCheck: List[Sensor] =
      Globals.repository.getVisualElements.flatMap {
        case p: Sensor
            if (
              p.name == dependency.pinger &&
                dependency.service.map(_ == p.serviceName).getOrElse(true) &&
                dependency.server.map(_ == p.serverName).getOrElse(true) &&
                dependency.serviceCheckMode.map(_ == p.mode).getOrElse(true)
            ) =>
          Some(p)
        case _ => None
      }
    if (pingersToCheck.length == 0) {
      errors = errors ::: List(
        "no dependencies found for description: %s".format(dependency))
    }
    pingersToCheck.foreach(ptc => {
      lastCheckBeginVerifier.map(m => {
        val lastCheckBegin =
          ptc.lastCheckBegin.map(lcb => lcb.getTime).getOrElse(-1L)
        if (!m.verify(lastCheckBegin))
          errors = errors ::: List(
            "lastCheckBegin (%s) failed verification %s for %s"
              .format(lastCheckBegin, m.describe, dependency))
      })
      lastUptimeVerifier.map(m => {
        val lastUptime = ptc.lastUptime.map(lu => lu.getTime).getOrElse(-1L)
        if (!m.verify(lastUptime))
          errors = errors ::: List(
            "lastUptime (%s) failed verification %s for %s".format(lastUptime,
                                                                   m.describe,
                                                                   dependency))
      })
      lastCheckVerifier.map(m => {
        val lastCheckFinish = ptc.lastCheck.map(lc => lc.getTime).getOrElse(-1L)
        if (!m.verify(lastCheckFinish))
          errors = errors ::: List(
            "lastCheckCompleted (%s) failed verification %s for %s"
              .format(lastCheckFinish, m.describe, dependency))
      })
      lastWhyVerifier.map(m => {
        val lastWhy = ptc.lastWhy.getOrElse("never run")
        if (!m.verify(lastWhy))
          errors = errors ::: List(
            "lastWhy (%s) failed verification %s for %s".format(lastWhy,
                                                                m.describe,
                                                                dependency))
      })
      lastDetailsVerifier.map(m => {
        val lastDetails = ptc.lastDetail.getOrElse("never failed")
        if (!m.verify(lastDetails))
          errors = errors ::: List(
            "lastDetails (%s) failed verification %s for %s".format(lastDetails,
                                                                    m.describe,
                                                                    dependency))
      })
      lastStatusVerifier.map(m => {
        val lastStatus = ptc.lastStatus.getOrElse(false)
        if (!m.verify(lastStatus))
          errors = errors ::: List(
            "lastStatus (%s) failed verification %s for %s".format(lastStatus,
                                                                   m.describe,
                                                                   dependency))
      })
      isRunningVerifier.map(m => {
        val isRunning = ptc.isRunning
        if (!m.verify(isRunning))
          errors = errors ::: List(
            "isRunning (%s) failed verification %s for %s".format(isRunning,
                                                                  m.describe,
                                                                  dependency))
      })
    })
    VerificationResponse(errors.length == 0, errors)
  }
}

object Matchers extends ConfigFileReader {
  import net.liftweb.json._
  import Serialization._
  protected implicit val formats = DefaultFormats
  def configureFromJson(jv: JValue): Map[String, Matcher] = {
    Map.empty[String, Matcher] //TODO
  }
  def configureFromXml(n: Node): Map[String, Matcher] = {
    Map(getNodes(n, "matcher").map(t => {
      val valueName = getAttr(t, "name").getOrElse("unknown")
      val matcher = configureVerificationFuncFromXml(t)
      (valueName, matcher)
    }): _*)
  }
  def configureVerificationFuncFromXml(n: Node): Matcher = {
    def getAttributeAs[B](in: Node,
                          attributeName: String,
                          converterFunc: String => B,
                          default: B): B = {
      getAttr(in, attributeName)
        .map(a => {
          try {
            converterFunc(a)
          } catch {
            case _: Throwable => default
          }
        })
        .getOrElse(default)
    }
    val noneFuncs = getImmediateNodes(n, "none").map(o => {
      val children = configureVerificationFuncFromXml(o)
      NoneMatcher(children.getSubMatchers)
    })
    val someFuncs = getImmediateNodes(n, "some").map(o => {
      val children = configureVerificationFuncFromXml(o)
      SomeMatcher(children.getSubMatchers)
    })
    val notSomeFuncs = getImmediateNodes(n, "notSome").map(o => {
      val children = configureVerificationFuncFromXml(o)
      SomeMatcher(children.getSubMatchers)
    })
    val someNotFuncs = getImmediateNodes(n, "someNot").map(o => {
      val children = configureVerificationFuncFromXml(o)
      SomeNotMatcher(children.getSubMatchers)
    })
    val allFuncs = getImmediateNodes(n, "all").map(a => {
      val children = configureVerificationFuncFromXml(a)
      AllMatcher(children.getSubMatchers)
    })
    val lessThanFuncs = getImmediateNodes(n, "lessThan").map(l => {
      val matcher = getAttributeAs[Double](l,
                                           "value",
                                           (s: String) => s.toString.toDouble,
                                           Double.NaN)
      IsLessThanMatcher(matcher)
    })
    val moreThanFuncs = getImmediateNodes(n, "greaterThan").map(m => {
      val matcher = getAttributeAs[Double](m,
                                           "value",
                                           (s: String) => s.toString.toDouble,
                                           Double.NaN)
      IsGreaterThanMatcher(matcher)
    })
    val equalsFuncs = getImmediateNodes(n, "numericEquals").map(e => {
      val matcher = getAttributeAs[Double](e,
                                           "value",
                                           (s: String) => s.toString.toDouble,
                                           Double.NaN)
      IsNumericallyEqualsMatcher(matcher)
    })
    val booleanEqualsFuncs = getImmediateNodes(n, "booleanEquals").map(l => {
      val matcher = getAttributeAs[Boolean](l,
                                            "value",
                                            (s: String) =>
                                              s.toLowerCase.trim match {
                                                case "true"  => true
                                                case "false" => false
                                                case _       => false
                                            },
                                            false)
      new IsBooleanEqualsMatcher(matcher)
    })
    val dependencyFuncs = getImmediateNodes(n, "dependency").map(o => {
      val pinger = getAttr(o, "check").getOrElse("unknown")
      val service = getAttr(o, "service")
      val server = getAttr(o, "server")
      val serviceCheckMode =
        getAttr(o, "serviceCheckMode").map(scm => ServiceCheckMode.parse(scm))
      val desc =
        DependencyDescription(pinger, server, service, serviceCheckMode)
      val matcher = DependencyMatchers.configureFromXml(o)
      DependencyCheckMatcher(desc, matcher)
    })
    val equalsStringFuncs = getImmediateNodes(n, "stringEquals").map(es => {
      val matcher = getAttr(es, "value").getOrElse("")
      IsStringEqualsMatcher(matcher)
    })
    val containsStringFuncs = getImmediateNodes(n, "stringContains").map(es => {
      val matcher = getAttr(es, "value").getOrElse("")
      IsStringContainsMatcher(matcher)
    })
    val caseInsensitiveContainsStringFuncs =
      getImmediateNodes(n, "stringContainsIgnoreCase").map(es => {
        val matcher = getAttr(es, "value").getOrElse("").toLowerCase.trim
        IsStringContainsIgnoreCaseMatcher(matcher)
      })
    val regexFuncs = getImmediateNodes(n, "regexMatches").map(es => {
      val matcher = getAttr(es, "value").getOrElse("")
      val regexPattern = matcher.r
      IsRegexMatchedMatcher(matcher, regexPattern)
    })
    val caseInsensitiveEqualsStringFuncs =
      getImmediateNodes(n, "stringEqualsIgnoreCase").map(es => {
        val matcher = getAttr(es, "value").getOrElse("").toLowerCase.trim
        IsStringEqualsIgnoreCaseMatcher(matcher)
      })
    AllMatcher(
      noneFuncs ::: someFuncs ::: someNotFuncs ::: allFuncs ::: lessThanFuncs ::: moreThanFuncs ::: equalsFuncs ::: booleanEqualsFuncs ::: equalsStringFuncs ::: containsStringFuncs ::: caseInsensitiveContainsStringFuncs ::: regexFuncs ::: caseInsensitiveEqualsStringFuncs ::: dependencyFuncs)
  }
}
