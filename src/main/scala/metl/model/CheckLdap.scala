package metl.model

import javax.naming.{Context, NamingEnumeration}
import javax.naming.directory.{InitialDirContext, SearchControls}
import net.liftweb.util.Helpers._

class PingLDAP(serviceCheckMode:ServiceCheckMode,serviceCheckSeverity:ServiceCheckSeverity,incomingName:String,incomingLabel:String,host:String,username:String,password:String,searchBase:String,searchTerm:String,time:TimeSpan = 5 seconds) extends Pinger(incomingName,incomingLabel,serviceCheckMode,serviceCheckSeverity){
  override val pollInterval = time
  private val infoGroups = Seq("cn","uid","sn","givenname","mail")
  private val env = new java.util.Hashtable[String,String]();
  env.put(Context.INITIAL_CONTEXT_FACTORY,"com.sun.jndi.ldap.LdapCtxFactory")
  env.put(Context.PROVIDER_URL,host)
  env.put(Context.SECURITY_AUTHENTICATION,"simple")
  env.put(Context.SECURITY_PRINCIPAL,username)
  env.put(Context.SECURITY_CREDENTIALS,password)
  private var ctx = new InitialDirContext(env)
  private val controls = new SearchControls
  controls.setSearchScope(SearchControls.SUBTREE_SCOPE)
  def status = {
    ctx.close
    ctx = new InitialDirContext(env)
    val results = ctx.search(searchBase,searchTerm,controls)
    val output = results.hasMore match {
      case true => {
        val rawAttributes = results.next().getAttributes()
        infoGroups.map(
          group => (group,rawAttributes.get(group))
        ).filter{
          case (name,attr) => attr != null
        }.map{
          case (name:String,attrib:javax.naming.directory.Attribute) => {
            attrib.getAll match {
              case namingEnum:NamingEnumeration[_] => {
                var mySeq = List.empty[(String,String)]
                while(namingEnum.hasMore())
                  mySeq = (name,namingEnum.next.toString) :: mySeq
                mySeq
              }
            }
          }
          case _ => ""
        }.toString
      }
      case false => "user (%s) not found in HDS".format(searchTerm)
    }
    results.close
    ctx.close
    output
  }
  override def performCheck = succeed(status.toString)
}
