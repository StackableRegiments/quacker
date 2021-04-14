package metl {
  package snippet {

    import _root_.scala.xml.{NodeSeq, Text}
    import _root_.net.liftweb.util._
    import _root_.net.liftweb.common._
    import _root_.java.util.Date
    import metl.lib._
    import Helpers._

    class UserSpecificActors {
      def dashboard(x: NodeSeq): NodeSeq =
        <span class={"lift:comet?type=Dashboard;name=%s".format(nextFuncName)} name="dashboard">{x}</span>
      def header(x: NodeSeq): NodeSeq =
        <span class={"lift:comet?type=Header;name=%s".format(nextFuncName)} name="header">{x}</span>
    }
  }
}
