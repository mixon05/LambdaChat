package webapp

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import upickle.default.*
import scala.scalajs.js.URIUtils
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

object UserSearch:

  case class Chat(id: String, userIds: List[String])
  object Chat {
    implicit val rw: ReadWriter[Chat] = macroRW
  }

  case class User(id: String, username: String)
  object User {
    implicit val rw: ReadWriter[User] = macroRW
  }

  def apply(): Element =
    val query     = Var("")
    val results   = Var(List.empty[User])
    val message   = Var("")
    val loading   = Var(false)
    val selected  = Var(Set.empty[User])

    def getTokenAndUserId: Option[(String, String)] =
      Option(dom.window.localStorage.getItem("token"))
        .zip(Option(dom.window.localStorage.getItem("userId")))

    def searchUsers(): Unit =
      val token = dom.window.localStorage.getItem("token")
      if token == null then
        message.set("Not logged in")
      else
        loading.set(true)
        Ajax.get(
          url = s"http://localhost:8080/users/search?q=${URIUtils.encodeURIComponent(query.now())}",
          headers = Map("Authorization" -> s"Bearer $token")
        ).map { xhr =>
          if xhr.status == 200 then
            Try(read[List[User]](xhr.responseText)).fold(
              ex => message.set(s"JSON decode error: ${ex.getMessage}"),
              users => {
                results.set(users)
                message.set("")
              }
            )
          else
            message.set(s"Error ${xhr.status}: ${xhr.responseText}")
        }.recover {
          case _ => message.set("Network error")
        }.foreach { _ =>
          loading.set(false)
        }

    def toggleUserSelection(user: User): Unit =
      val current = selected.now()
      if current.contains(user) then selected.set(current - user)
      else selected.set(current + user)

    def createChatWith(user: User): Unit =
      createChat(List(user.id))

    def createGroupChat(): Unit =
      val userIds = selected.now().map(_.id).toList
      if userIds.nonEmpty then
        createChat(userIds)

    def createChat(userIds: List[String]): Unit =
      getTokenAndUserId.fold {
        message.set("Not logged in")
      } { case (token, userId) =>
        loading.set(true)
        Ajax.get(
          url = s"http://localhost:8080/users/$userId/chats",
          headers = Map("Authorization" -> s"Bearer $token")
        ).map { xhr =>
          if xhr.status == 200 then
            try
              val chats = read[List[Chat]](xhr.responseText)
              val matched = chats.find(chat => userIds.toSet.subsetOf(chat.userIds.toSet) && chat.userIds.toSet.subsetOf(userIds.toSet + userId))
              matched match
                case Some(existingChat) =>
                  Main.navigateTo(s"/chat/${existingChat.id}")
                case None =>
                  val json = write(Map("userIds" -> userIds))
                  Ajax.post(
                    url = "http://localhost:8080/chats",
                    data = json,
                    headers = Map(
                      "Content-Type" -> "application/json",
                      "Authorization" -> s"Bearer $token"
                    )
                  ).map { xhr =>
                    if xhr.status == 200 then
                      val resp = js.JSON.parse(xhr.responseText).asInstanceOf[js.Dynamic]
                      val chatId = resp.chatId.asInstanceOf[String]
                      Main.navigateTo(s"/chat/$chatId")
                    else
                      message.set(s"Error: ${xhr.status}")
                  }.recover {
                    case _ => message.set("Network error")
                  }
            catch case ex: Throwable =>
              message.set(s"JSON decode error: ${ex.getMessage}")
          else
            message.set(s"Server error: ${xhr.status}")
        }.recover {
          case _ => message.set("Network error")
        }.foreach { _ =>
          loading.set(false)
        }
      }

    def renderSpinner(loading: Signal[Boolean]) =
      div(
        cls := "spinner",
        display <-- loading.map(if _ then "block" else "none"),
        "Loading..."
      )

    def renderError(message: Signal[String]) =
      child.maybe <-- message.map {
        case "" => None
        case msg => Some(div(cls := "error-message", msg))
      }

    div(
      onMountCallback(_ => searchUsers()),
      cls := "user-search-container",
      h2("Search Users"),
      input(
        placeholder := "Search...",
        cls := "search-input",
        controlled(
          value <-- query,
          onInput.mapToValue --> query
        )
      ),
      button(
        "Search",
        cls := "search-button",
        onClick --> { _ => searchUsers() }
      ),
      renderSpinner(loading.signal),
      ul(
        cls := "user-list",
        children <-- results.signal.map(_.map { user =>
          li(
            cls := "user-item",
            onClick --> { _ => toggleUserSelection(user) },
            child <-- selected.signal.map(sel =>
              if sel.contains(user) then b(user.username)
              else span(user.username)
            ),
            button("Chat", onClick.stopPropagation.mapTo(user) --> { u => createChatWith(u) })
          )
        })
      ),
      button(
        "Create Group Chat",
        cls := "group-chat-button",
        onClick --> { _ => createGroupChat() },
        disabled <-- selected.signal.map(_.isEmpty)
      ),
      renderError(message.signal)
    )
