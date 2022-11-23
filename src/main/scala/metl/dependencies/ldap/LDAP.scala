package com.metl.ldap

import com.metl.utils._

import org.apache.commons.io._
import javax.naming._
import javax.naming.directory._
import net.liftweb.common._
import net.liftweb.util.Helpers._

case class GroupDefinition(groupName: String,
                           owners: List[String],
                           members: List[String])

trait IMeTLLDAP {
  def authenticate(username: String, password: String): Option[Boolean]
  def fullQuery(attrName: String,
                attrValue: String): List[Seq[Tuple2[String, String]]]
  def simpleQuery(attrName: String,
                  attrValue: String,
                  returnType: String): List[String]
  def getValuePairs(attrName: String,
                    attrValue: String,
                    returnTypes: List[String])
    : List[Tuple2[String, Seq[Tuple2[String, String]]]]
  def getGroupDefinition(groupName: String): Option[GroupDefinition]
  def getGroupsFor(username: String): List[GroupDefinition]
}

object DisconnectedLDAP extends IMeTLLDAP {
  override def authenticate(username: String, password: String) = None
  override def fullQuery(attrName: String,
                         attrValue: String): List[Seq[Tuple2[String, String]]] =
    List.empty[Seq[Tuple2[String, String]]]
  override def simpleQuery(attrName: String,
                           attrValue: String,
                           returnType: String): List[String] =
    List.empty[String]
  override def getValuePairs(attrName: String,
                             attrValue: String,
                             returnTypes: List[String])
    : List[Tuple2[String, Seq[Tuple2[String, String]]]] =
    List.empty[Tuple2[String, Seq[Tuple2[String, String]]]]
  def getGroupDefinition(groupName: String): Option[GroupDefinition] = None
  override def getGroupsFor(username: String): List[GroupDefinition] =
    List.empty[GroupDefinition]
}

trait LDAPSearch {
  val bindBase: String
  def withLDAP(searchTerm: String, action: (List[SearchResult]) => Unit): Unit
  def withLDAPUsingCredentials(user: String,
                               password: String,
                               searchTerm: String,
                               action: (List[SearchResult]) => Unit,
                               base: String,
                               usernameIncludesBase: Boolean): Unit
}

class LDAPSearchService(directory: String,
                        incomingBindBase: String,
                        bindUser: String,
                        password: String,
                        secure: Boolean = false)
    extends LDAPSearch
    with Logger {
  val bindBase = incomingBindBase
  def withLDAPUsingCredentials(user: String,
                               pass: String,
                               searchTerm: String,
                               action: (List[SearchResult]) => Unit,
                               base: String = bindBase,
                               usernameIncludesBase: Boolean = true): Unit =
    Stopwatch.time(
      "LDAP.withLDAPUsingCredentials", {
        var env = new java.util.Hashtable[String, String]()
        val constructedUsername = usernameIncludesBase match {
          case true  => user
          case false => "%s, %s".format(user, base)
        }
        env.put(Context.INITIAL_CONTEXT_FACTORY,
                "com.sun.jndi.ldap.LdapCtxFactory")
        env.put(Context.PROVIDER_URL, directory)
        if (secure) {
          env.put(Context.SECURITY_PROTOCOL, "ssl")
        }
        env.put(Context.SECURITY_AUTHENTICATION, "simple")
        env.put(Context.SECURITY_PRINCIPAL, constructedUsername)
        env.put(Context.SECURITY_CREDENTIALS, pass)
        try {
          val ctx = new InitialDirContext(env)
          val controls = new SearchControls
          controls.setSearchScope(SearchControls.SUBTREE_SCOPE)
          val results = ctx.search(base, searchTerm, controls)
          var resultEnumerations = List.empty[SearchResult]
          while (results.hasMore) {
            resultEnumerations = results.next() :: resultEnumerations
          }
          results.close
          ctx.close
          action(resultEnumerations)
        } catch {
          case e: Throwable => {
            error("ldap search exception", e)
          }
        }
      }
    )

  def withLDAP(searchTerm: String, action: (List[SearchResult]) => Unit): Unit =
    Stopwatch.time("LDAP.withLDAP", {
      withLDAPUsingCredentials(bindUser,
                               password,
                               searchTerm,
                               action,
                               bindBase,
                               false)
    })
}

class LDAPService(env: { val ldapSearch: LDAPSearch }) {
  val searchService = env.ldapSearch
  def withLDAP(searchTerm: String,
               action: (List[SearchResult]) => Unit): Unit = {
    env.ldapSearch.withLDAP(searchTerm, action)
  }
  def withLDAPUsingCredentials(username: String,
                               password: String,
                               searchTerm: String,
                               action: (List[SearchResult]) => Unit,
                               base: String,
                               usernameIncludesBase: Boolean): Unit = {
    env.ldapSearch.withLDAPUsingCredentials(username,
                                            password,
                                            searchTerm,
                                            action,
                                            base,
                                            usernameIncludesBase)
  }
}

class LDAPConfig(directory: String,
                 bindUser: String,
                 password: String,
                 bindBase: String,
                 secure: Boolean = false) {
  lazy val ldapSearch =
    new LDAPSearchService(directory, bindBase, bindUser, password, secure)
  lazy val ldap = new LDAPService(this)
}

class LDAP(env: { val ldap: LDAPService }) extends IMeTLLDAP {
  protected val usernameCheckType = "uid"
  override def authenticate(username: String,
                            password: String): Option[Boolean] =
    Stopwatch.time(
      "LDAP.authenticate", {
        var recordOutput: Option[String] = None
        var output: Option[Boolean] = None
        val func = (n: List[SearchResult]) => {
          recordOutput = n.take(1).map(ne => ne.getName).headOption
        }
        env.ldap.withLDAP("(%s=%s)".format(usernameCheckType, username), func)
        val func2 = (n: List[SearchResult]) => {
          output = Some(n.length > 0)
        }
        recordOutput.map(dn => {
          env.ldap.withLDAPUsingCredentials(
            dn,
            password,
            "(%s=%s)".format(usernameCheckType, username),
            func2,
            env.ldap.searchService.bindBase,
            false)
        })
        output
      }
    )
  override def fullQuery(attrName: String,
                         attrValue: String): List[Seq[Tuple2[String, String]]] =
    Stopwatch.time(
      "LDAP.fullQuery", {
        var output = List.empty[Seq[Tuple2[String, String]]]
        val func = (n: List[SearchResult]) => {
          output = n
            .map(ne => {
              try {
                Some(
                  listFromNamingEnum[Attribute, Attribute](
                    ne.getAttributes()
                      .getAll
                      .asInstanceOf[NamingEnumeration[Attribute]],
                    a => a)
                    .map(a => {

                      val fullDn =
                        "%s, %s".format(ne.getName,
                                        env.ldap.searchService.bindBase)
                      ("dn", fullDn) :: namingEnumerationToSeq(a.getID,
                                                               a.getAll).toList
                    })
                    .flatten)
              } catch {
                case e: Throwable => None
              }
            })
            .flatten
        }
        env.ldap.withLDAP("(%s=%s)".format(attrName, attrValue), func)
        output
      }
    )
  override def simpleQuery(attrName: String,
                           attrValue: String,
                           returnType: String): List[String] =
    Stopwatch.time(
      "LDAP.simpleQuery", {
        var output = List.empty[String]
        val func = (n: List[SearchResult]) => {
          output = n
            .map(
              ne =>
                tryo(
                  namingEnumerationToSeq(
                    returnType,
                    ne.getAttributes().get(returnType).getAll)
                    .map(a => a._2)).openOr(List.empty[String]).toList)
            .flatten
        }
        env.ldap.withLDAP("(%s=%s)".format(attrName, attrValue), func)
        output
      }
    )
  override def getValuePairs(attrName: String,
                             attrValue: String,
                             returnTypes: List[String])
    : List[Tuple2[String, Seq[Tuple2[String, String]]]] =
    Stopwatch.time(
      "LDAP.getValuePairs", {
        var output: List[Tuple2[String, Seq[Tuple2[String, String]]]] =
          List.empty[Tuple2[String, Seq[Tuple2[String, String]]]]
        val func = (n: List[SearchResult]) => {
          output = n.map(net => {
            val ne = net.asInstanceOf[SearchResult]
            Tuple2(ne.getName,
                   returnTypes
                     .map(group => getSeqOfValuesFromSearchResult(ne, group))
                     .flatten)
          })
        }
        env.ldap.withLDAP("(%s=%s)".format(attrName, attrValue), func)
        output
      }
    )
  protected def listFromNamingEnum[A, B](
      namingEnumeration: NamingEnumeration[A],
      transformer: A => B): List[B] =
    Stopwatch.time(
      "LDAP.listFromNamingEnum", {
        var mySeq = List.empty[B]
        while (namingEnumeration.hasMoreElements()) {
          Option(namingEnumeration.nextElement).foreach(v =>
            mySeq = mySeq ::: List(transformer(v)))
        }
        mySeq
      }
    )
  protected def namingEnumerationToSeq[A](
      name: String,
      namingEnumeration: NamingEnumeration[A]): Seq[(String, String)] =
    Stopwatch.time("LDAP.namingEnumerationToSeq", {
      listFromNamingEnum[A, String](namingEnumeration, n => n.toString).map(n =>
        (name, n))
    })
  protected def getSeqOfValuesFromSearchResult(
      in: SearchResult,
      requestedValue: String): Seq[Tuple2[String, String]] = {
    List(in.getAttributes().get(requestedValue))
      .filter { case attr => attr != null }
      .map {
        case attrib: Attribute =>
          namingEnumerationToSeq(requestedValue, attrib.getAll)
      }
      .flatten
  }
  protected val ownerKey = "owner"
  protected val memberKey = "uniquemember"
  override def getGroupDefinition(groupName: String): Option[GroupDefinition] =
    Stopwatch.time(
      "LDAP.getGroupDefinition", {
        var output: Option[GroupDefinition] = None
        val func = (n: List[SearchResult]) => {
          output = n.headOption.map(ho => {
            val owners = getSeqOfValuesFromSearchResult(ho, ownerKey)
              .filter(i => i._1 == ownerKey)
              .map(i => i._2)
              .toList
            val members = getSeqOfValuesFromSearchResult(ho, memberKey)
              .filter(i => i._1 == memberKey)
              .map(i => i._2)
              .toList
            GroupDefinition(groupName, owners, members)
          })
        }
        env.ldap.withLDAP("(cn=%s)".format(groupName), func)
        output
      }
    )
  override def getGroupsFor(username: String): List[GroupDefinition] =
    Stopwatch.time("LDAP.getGroupsFor", {
      fullQuery("uid", username)
        .map(sr => {
          val dn = sr.find(i => i._1 == "dn").map(i => i._2)
          dn.map(getGroupsForDn(_))
        })
        .flatten
        .flatten
    })
  protected def getGroupsForDn(dn: String): List[GroupDefinition] =
    Stopwatch.time(
      "LDAP.getGroupsForDn", {
        val filter = "(&((objectclass=groupOfUniqueNames)(|((%s=%s)(%s=%s)))))"
          .format(ownerKey, dn, memberKey, dn)
        getGroupsForDnWithFilter(dn, filter)
      }
    )
  protected def getGroupsForDnWithFilter(
      dn: String,
      filterString: String): List[GroupDefinition] =
    Stopwatch.time(
      "LDAP.getGroupsForDnWithFilter", {
        var output = List.empty[GroupDefinition]
        val func = (n: List[SearchResult]) => {
          output = n
            .map(ne => {
              val groupName =
                getSeqOfValuesFromSearchResult(ne, "cn").map(i => i._2).head
              val owners = getSeqOfValuesFromSearchResult(ne, ownerKey)
                .filter(i => i._1 == ownerKey)
                .map(i => i._2)
                .toList
              val members = getSeqOfValuesFromSearchResult(ne, memberKey)
                .filter(i => i._1 == memberKey)
                .map(i => i._2)
                .toList
              GroupDefinition(groupName, owners, members)
            })
            .toList
        }
        env.ldap.withLDAP(filterString, func)
        output
      }
    )
}
