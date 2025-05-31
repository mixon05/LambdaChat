package webapp

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import upickle.default.*
import scala.scalajs.js.URIUtils
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global

object UserSearch:

  def apply(): Element =
    val query   = Var("")
    val results = Var(List.empty[User])
    val message = Var("")

    def searchUsers(): Unit =
      val token = dom.window.localStorage.getItem("token")
      if token == null then
        message.set("Not logged in")
      else
        Ajax.get(
          url = s"http://localhost:8080/users/search?q=${URIUtils.encodeURIComponent(query.now())}",
          headers = Map("Authorization" -> s"Bearer $token")
        ).map { xhr =>
          if xhr.status == 200 then
            try
              val users = read[List[User]](xhr.responseText)
              results.set(users)
            catch case ex: Throwable =>
              message.set(s"JSON decode error: ${ex.getMessage}")
          else
            message.set(s"Error ${xhr.status}: ${xhr.responseText}")
        }.recover {
          case _ => message.set("Network error")
        }

    def createChatWith(user: User): Unit =
      val token = dom.window.localStorage.getItem("token")
      val currentUser = dom.window.localStorage.getItem("username")
      if token == null || currentUser == null then
        message.set("Not logged in")
      else
        val json = s"""{"userIds": ["${user.id}"]}"""
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
            dom.window.location.href = s"/chat/$chatId"
          else
            message.set(s"Error: ${xhr.status}")
        }.recover {
          case _ => message.set("Network error")
        }

    div(
      h2("Search Users"),
      input(
        placeholder := "Search...",
        controlled(
          value <-- query,
          onInput.mapToValue --> query
        )
      ),
      button("Search", onClick --> { _ => searchUsers() }),
      div(cls := "search-message", child.text <-- message.signal),
      ul(
        children <-- results.signal.map(_.map { user =>
          li(
            user.username,
            onClick --> { _ => createChatWith(user) },
            cls := "user-item"
          )
        })
      )
    )

  case class User(id: String, username: String)

  object User {
    implicit val rw: ReadWriter[User] = macroRW
  }
