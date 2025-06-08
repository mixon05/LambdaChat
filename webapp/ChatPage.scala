package webapp

import org.scalajs.dom.html
import org.scalajs.dom
import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.concurrent.ExecutionContext.Implicits.global
import upickle.default.*
import scala.util.Try

object ChatPage:

  case class Message(id: String, chatId: String, senderId: String, value: String)
  object Message:
    implicit val rw: ReadWriter[Message] = macroRW

  def apply(chatId: String): Element =
    val messages       = Var(List.empty[Message])
    val newMessage     = Var("")
    val messageStatus  = Var("")
    val loading        = Var(false)

    def getAuthTokenAndUserId: Option[(String, String)] =
      Option(dom.window.localStorage.getItem("token"))
        .zip(Option(dom.window.localStorage.getItem("userId")))

    def renderMessage(msg: Message): Element =
      val currentUserId = dom.window.localStorage.getItem("userId")
      val clsName =
        if msg.senderId == currentUserId then "message-item message-right"
        else "message-item message-left"
      li(s"${msg.senderId}: ${msg.value}", cls := clsName)

    def fetchMessages(): Unit =
      getAuthTokenAndUserId.fold {
        messageStatus.set("Not logged in")
      } { case (token, userId) =>
        loading.set(true)
        Ajax.get(
          url = s"http://localhost:8080/chats/$chatId/messages",
          headers = Map("Authorization" -> s"Bearer $token")
        ).map { xhr =>
          if xhr.status == 200 then
            Try(read[List[Message]](xhr.responseText)).fold(
              ex => messageStatus.set(s"Decode error: ${ex.getMessage}"),
              msgs => {
                messages.set(msgs)
                messageStatus.set("")
              }
            )
          else
            messageStatus.set(s"Server error: ${xhr.status}")
        }.recover {
          case _ => messageStatus.set("Network error")
        }.foreach { _ =>
          loading.set(false)
        }
      }

    def sendMessage(): Unit =
      getAuthTokenAndUserId.fold {
        messageStatus.set("Not logged in")
      } { case (token, senderId) =>
        val body = write(Map("chatId" -> chatId, "value" -> newMessage.now()))

        Ajax.post(
          url = "http://localhost:8080/messages",
          data = body,
          headers = Map(
            "Content-Type" -> "application/json",
            "Authorization" -> s"Bearer $token"
          )
        ).map { xhr =>
          if xhr.status == 200 then
            newMessage.set("")
            fetchMessages()
          else
            messageStatus.set(s"Error: ${xhr.status}")
        }.recover {
          case _ => messageStatus.set("Network error")
        }
      }

    div(
      onMountCallback(_ => fetchMessages()),
      cls := "chat-container",
      div(cls := "chat-header", h2(s"Chat: $chatId")),
      div(
        cls := "messages-section",
        child.maybe <-- loading.signal.map {
          case true  => Some(div(cls := "spinner", "Loading..."))
          case false => None
        },
        ul(
          cls := "messages-list",
          inContext { thisNode =>
            messages.signal.map { _ =>
              dom.window.requestAnimationFrame { _ =>
                thisNode.ref.scrollTop = thisNode.ref.scrollHeight
              }
            } --> Observer.empty
          },
          children <-- messages.signal.map(_.map(renderMessage))
        )
      ),
      form(
        cls := "message-form",
        onSubmit.preventDefault --> { _ => sendMessage() },
        input(
          cls := "message-input",
          placeholder := "Type a message...",
          controlled(
            value <-- newMessage,
            onInput.mapToValue --> newMessage
          )
        ),
        button("Send", tpe := "submit", cls := "send-button")
      )
    )