package metl.model

import net.liftweb._
import util._
import common._

trait Repository extends Logger {
  def updateVisualElement(id: String, v: Option[VisualElement]): Unit
  def updateNotifier(id: String, v: Option[ErrorActor]): Unit
  def updateValidUser(id: String, v: Option[UserAccessRestriction]): Unit
  def updateHistoryListener(id: String, v: Option[HistoryListener]): Unit
  def getVisualElement(id: String): Option[VisualElement]
  def getNotifier(id: String): Option[ErrorActor]
  def getValidUser(id: String): Option[UserAccessRestriction]
  def getHistoryListener(id: String): Option[HistoryListener]
  def getVisualElements: List[VisualElement]
  def getNotifiers: List[ErrorActor]
  def getValidUsers: List[UserAccessRestriction]
  def getHistoryListeners: List[HistoryListener]
  def startup: Unit = {}
  def shutdown: Unit = {}
}

object NullRepository extends Repository {
  def updateVisualElement(id: String, v: Option[VisualElement]): Unit = ???
  def updateNotifier(id: String, v: Option[ErrorActor]): Unit = ???
  def updateValidUser(id: String, v: Option[UserAccessRestriction]): Unit = ???
  def updateHistoryListener(id: String, v: Option[HistoryListener]): Unit = ???
  def getVisualElement(id: String): Option[VisualElement] = None
  def getNotifier(id: String): Option[ErrorActor] = None
  def getValidUser(id: String): Option[UserAccessRestriction] = None
  def getHistoryListener(id: String): Option[HistoryListener] = None
  def getVisualElements: List[VisualElement] = Nil
  def getNotifiers: List[ErrorActor] = Nil
  def getValidUsers: List[UserAccessRestriction] = Nil
  def getHistoryListeners: List[HistoryListener] = Nil
}

class RepositoryWrapper(wrapped: Repository) extends Repository {
  def updateVisualElement(id: String, v: Option[VisualElement]): Unit =
    wrapped.updateVisualElement(id, v)
  def updateNotifier(id: String, v: Option[ErrorActor]): Unit =
    wrapped.updateNotifier(id, v)
  def updateValidUser(id: String, v: Option[UserAccessRestriction]): Unit =
    wrapped.updateValidUser(id, v)
  def updateHistoryListener(id: String, v: Option[HistoryListener]): Unit =
    wrapped.updateHistoryListener(id, v)
  def getVisualElement(id: String): Option[VisualElement] =
    wrapped.getVisualElement(id)
  def getNotifier(id: String): Option[ErrorActor] = wrapped.getNotifier(id)
  def getValidUser(id: String): Option[UserAccessRestriction] =
    wrapped.getValidUser(id)
  def getHistoryListener(id: String): Option[HistoryListener] =
    wrapped.getHistoryListener(id)
  def getVisualElements: List[VisualElement] = wrapped.getVisualElements
  def getNotifiers: List[ErrorActor] = wrapped.getNotifiers
  def getValidUsers: List[UserAccessRestriction] = wrapped.getValidUsers
  def getHistoryListeners: List[HistoryListener] = wrapped.getHistoryListeners
  override def startup = wrapped.startup
  override def shutdown = wrapped.shutdown
}

class InMemoryRepository extends Repository {
  class SyncedKeyedMap[A] {
    protected val store = new scala.collection.mutable.HashMap[String, A]()
    private val lockObj = new Object()
    def getAll: List[A] = lockObj.synchronized {
      store.values.toList
    }
    def update(k: String, v: Option[A]): Unit = lockObj.synchronized {
      v.map(vi => {
          store += ((k, vi))
        })
        .getOrElse({
          store - k
        })
    }
    def get(k: String): Option[A] = lockObj.synchronized {
      store.get(k)
    }
  }
  protected val validUsers = new SyncedKeyedMap[UserAccessRestriction]()
  protected val visualElements = new SyncedKeyedMap[VisualElement]()
  protected val historyListeners = new SyncedKeyedMap[HistoryListener]()
  protected val notifiers = new SyncedKeyedMap[ErrorActor]()
  def updateVisualElement(id: String, v: Option[VisualElement]): Unit =
    visualElements.update(id, v)
  def updateNotifier(id: String, v: Option[ErrorActor]): Unit =
    notifiers.update(id, v)
  def updateValidUser(id: String, v: Option[UserAccessRestriction]): Unit =
    validUsers.update(id, v)
  def updateHistoryListener(id: String, v: Option[HistoryListener]): Unit =
    historyListeners.update(id, v)
  def getVisualElement(id: String): Option[VisualElement] =
    visualElements.get(id)
  def getNotifier(id: String): Option[ErrorActor] = notifiers.get(id)
  def getValidUser(id: String): Option[UserAccessRestriction] =
    validUsers.get(id)
  def getHistoryListener(id: String): Option[HistoryListener] =
    historyListeners.get(id)
  def getVisualElements: List[VisualElement] = visualElements.getAll
  def getNotifiers: List[ErrorActor] = notifiers.getAll
  def getValidUsers: List[UserAccessRestriction] = validUsers.getAll
  def getHistoryListeners: List[HistoryListener] = historyListeners.getAll
}
