package webapp

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.concurrent.ExecutionContext.Implicits.global

object ChatList:

  case class Chat(id: String, userIds: List[String])

  object Chat {
    implicit val rw: upickle.default.ReadWriter[Chat] = upickle.default.macroRW
  }

  def apply(): Element =
    val message   = Var("")
    val chatList  = Var(List.empty[Chat])

    def fetchChats(): Unit =
      val token = dom.window.localStorage.getItem("token")
      val userId = dom.window.localStorage.getItem("userId")
      if token == null || userId == null then
        message.set("Not logged in")
      else
        Ajax.get(
          url = s"http://localhost:8080/users/$userId/chats",
          headers = Map("Authorization" -> s"Bearer $token")
        ).map { xhr =>
          if xhr.status == 200 then
            try
              val chats = upickle.default.read[List[Chat]](xhr.responseText)
              chatList.set(chats)
            catch case ex: Throwable =>
              message.set(s"JSON decode error: ${ex.getMessage}")
          else
            message.set(s"Server error: ${xhr.status}")
        }.recover {
          case _ => message.set("Network error")
        }

    onMountCallback(_ => fetchChats())

    div(
      h2("Your Chats"),
      button("Refresh", onClick --> { _ => fetchChats() }),
      div(cls := "chat-message", child.text <-- message.signal),
      ul(
        children <-- chatList.signal.map(_.map { chat =>
          li(
            a(
              href := s"/chat/${chat.id}",
              s"Chat ID: ${chat.id}, users: ${chat.userIds.mkString(", ")}"
            )
          )
        })
      )
    )