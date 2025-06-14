package webapp

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import scala.concurrent.ExecutionContext.Implicits.global
import upickle.default.*

object ChatList:

  case class Chat(id: String, userIds: List[String], userNames: List[String])
  object Chat {
    implicit val rw: ReadWriter[Chat] = macroRW
  }

  def apply(): Element =
    val message    = Var("")
    val chatList   = Var(List.empty[Chat])
    val loading    = Var(false)

    def renderChatItem(chat: Chat): Element =
      li(
        cls := "chat-item",
        button(
          cls := "chat-link-button",
          onClick --> { _ => Main.navigateTo(s"/chat/${chat.id}") },
          s"Chat with: ${chat.userNames.drop(1).mkString(", ")}"
        )
      )

    def fetchChats(): Unit =
      val token = dom.window.localStorage.getItem("token")
      val userId = dom.window.localStorage.getItem("userId")

      Option(token).zip(Option(userId)).fold {
        message.set("Not logged in")
      } { case (tok, uid) =>
        loading.set(true)
        Ajax.get(
          url = s"http://localhost:8080/users/$uid/chats",
          headers = Map("Authorization" -> s"Bearer $tok")
        ).map { xhr =>
          if xhr.status == 200 then
            try
              val chats = read[List[Chat]](xhr.responseText)
              chatList.set(chats)
              message.set("")
            catch case ex: Throwable =>
              message.set(s"JSON decode error: ${ex.getMessage}")
          else
            message.set(s"Server error: ${xhr.status}")
        }.recover {
          case _ => message.set("Network error")
        }.foreach(_ => loading.set(false))
      }

    div(
      onMountCallback(_ => fetchChats()),
      cls := "chat-list-container",
      h2("Your Chats"),
      button(
        "Refresh",
        cls := "refresh-button",
        onClick --> { _ => fetchChats() }
      ),
      child.maybe <-- loading.signal.map {
        case true  => Some(div(cls := "spinner", "Loading chats..."))
        case false => None
      },
      child.maybe <-- message.signal.map {
        case "" => None
        case msg => Some(div(cls := "chat-message", msg))
      },
      ul(
        cls := "chat-list",
        children <-- chatList.signal.map {
          case Nil => List(li(cls := "empty-message", "You have no active chats."))
          case list => list.map(renderChatItem)
        }
      )
    )