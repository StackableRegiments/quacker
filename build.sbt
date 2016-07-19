name := "Quacker"
version := "0.0.1"
organization := "stackableRegiments"

val scalaVersionString = "2.11.5"

scalaVersion := scalaVersionString

resolvers ++= Seq(
  "snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"        at "http://oss.sonatype.org/content/repositories/releases",
  "mavenCentral"  at  "http://mvnrepository.com/artifact",
  "oosnmp" at "https://oosnmp.net/dist/release"
)

unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

scalacOptions ++= Seq("-deprecation", "-unchecked")

javaOptions in container ++= Seq(
  "-Drun.mode=development",
  "-Dmetlx.configurationFile=config/configuration.local.xml",
  "-Dquacker.configDirectoryLocation=config",
  "-Dlogback.configurationFile=config/logback.xml",
  "-XX:+UseConcMarkSweepGC",
  "-XX:+CMSClassUnloadingEnabled",
  "-Xmx256m",
  "-Xms256m"
)


libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.+"

libraryDependencies ++= {
  val liftVersion = "2.6.2"
  val shiroVersion = "1.2.2"
  val servletVersion = "2.5"
  val jettyVersion = "9.2.4.v20141103"
  Seq(
    /* Monitoring*/
   /*oracle*/
//    "com.oracle" % "ojdbc6" % "11.2.0.4",
    /*telnet & munin*/
    "commons-net" % "commons-net" % "2.0",
    /*snmp*/
    "org.snmp4j" % "snmp4j" % "1.11.5",
    /*mongo*/
    "org.mongodb" % "mongo-java-driver" % "2.6.3",
    /*html-cleaner for html parsing*/
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.9",
    /*mysql*/
    "mysql" % "mysql-connector-java" % "5.1.6",
    /*subversion*/
    "org.tmatesoft.svnkit" % "svnkit" % "1.3.4",
    /*general*/
    "commons-io" % "commons-io" % "1.4",
    "commons-codec" % "commons-codec" % "1.9",
    /*http*/
    "org.apache.httpcomponents" % "httpcore" % "4.1.2",
    /*memcached*/
    //"spy" % "spymemcached" % "2.6",
    "net.spy" % "spymemcached" % "2.12.1",
    /*h2*/
    "com.h2database" % "h2" % "1.4.187",
    /*xmpp*/
    "jivesoftware" % "smack" % "3.1.0",
    "jivesoftware" % "smackx" % "3.1.0",
    /*samba*/
    "jcifs" % "jcifs" % "1.3.17",
    /*general utils specific*/
    "io.github.stackableregiments" %% "common-utils" % "0.2.+",
//    "com.metl" % "LiftExtensions" % "1.0-SNAPSHOT",
    /*auth*/
    "io.github.stackableregiments" %% "lift-authentication" % "0.2.+",
    "io.github.stackableregiments" %% "cas-authentication" % "0.2.+",
    
    
    /*Lift framework*/
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-mapper"        % liftVersion        % "compile",
    "net.liftweb" %% "lift-mongodb" % liftVersion,
    "net.liftweb" %% "lift-mongodb-record" % liftVersion,
    "org.mongodb" %% "casbah" % "2.8.2",
    /*Standalone web server*/
    "org.eclipse.jetty" % "jetty-webapp"        % jettyVersion   % "container,test",
    "org.eclipse.jetty" % "jetty-plus"          % jettyVersion  % "container,test",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    /*Testing*/
    "org.specs2"        %% "specs2"             % "2.3.12"             % "test",
    /*Security stack*/
    "org.apache.shiro" % "shiro-core" % shiroVersion,
    "org.apache.shiro" % "shiro-cas" % shiroVersion,
    "org.apache.shiro" % "shiro-web" % shiroVersion,
    /*Service stack*/
    "javax.servlet" % "servlet-api" % servletVersion,
    /*Adhoc channels*/
    "io.github.stackableregiments" %% "ldap" % "0.2.+",
    /*Http management*/
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.+",
    /*Parsing*/
    "com.github.tototoshi" %% "scala-csv" % "1.2.1",
    /*OAuth authentication*/
    "org.pac4j" % "pac4j-oauth" % "1.7.0"
  )
}.map(_.excludeAll(ExclusionRule(organization = "org.slf4j")).exclude("com.sun.jdmk","jmxtools").exclude("javax.jms","jms").exclude("com.sun.jmx","jmxri"))

javacOptions ++= Seq("-source", "1.5", "-target", "1.5")

// append -deprecation to the options passed to the Scala compiler
scalacOptions += "-deprecation"

// define the repository to publish to
publishTo := Some("sonatype" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

// set Ivy logging to be at the highest level
ivyLoggingLevel := UpdateLogging.Full

// disable updating dynamic revisions (including -SNAPSHOT versions)
offline := false

// set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

// set the prompt (for the current project) to include the username
shellPrompt := { state => System.getProperty("user.name") + "> " }

// disable printing timing information, but still print [success]
showTiming := true

// disable printing a message indicating the success or failure of running a task
showSuccess := true

// change the format used for printing task completion time
timingFormat := {
  import java.text.DateFormat
  DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT)
}

testOptions in Test += Tests.Argument("-eI")


// don't aggregate clean (See FullConfiguration for aggregation details)
aggregate in clean := false

// only show warnings and errors on the screen for compilations.
//  this applies to both test:compile and compile and is Info by default
logLevel in compile := Level.Warn

// only show warnings and errors on the screen for all tasks (the default is Info)
//  individual tasks can then be more verbose using the previous setting
logLevel := Level.Warn

// only store messages at info and above (the default is Debug)
//   this is the logging level for replaying logging with 'last'
//persistLogLevel := Level.Debug

// only show 10 lines of stack traces
traceLevel := 10

// only show stack traces up to the first sbt stack frame
traceLevel := 0

credentials += Credentials(Path.userHome / ".ivy2" / "ivy-credentials")
