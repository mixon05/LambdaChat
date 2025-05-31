package webapp

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.concurrent.ExecutionContext.Implicits.global
import upickle.default.*

object ChatPage:

  case class Message(id: String, chatId: String, senderId: String, value: String)
  object Message:
    implicit val rw: ReadWriter[Message] = macroRW

  def apply(chatId: String): Element =
    val messages = Var(List.empty[Message])
    val newMessage = Var("")
    val messageStatus = Var("")

    def fetchMessages(): Unit =
      val token = dom.window.localStorage.getItem("token")
      if token == null then
        messageStatus.set("Not logged in")
      else
        Ajax.get(
          url = s"http://localhost:8080/chats/$chatId/messages",
          headers = Map("Authorization" -> s"Bearer $token")
        ).map { xhr =>
          if xhr.status == 200 then
            try
              val msgs = read[List[Message]](xhr.responseText)
              messages.set(msgs)
            catch case ex: Throwable =>
              messageStatus.set(s"Decode error: ${ex.getMessage}")
          else
            messageStatus.set(s"Server error: ${xhr.status}")
        }.recover { case _ => messageStatus.set("Network error") }

    def sendMessage(): Unit =
      val token = dom.window.localStorage.getItem("token")
      val senderId = dom.window.localStorage.getItem("userId")
      if token == null || senderId == null then
        messageStatus.set("Not logged in")
      else
        val body = s"""{
                      |  \"chatId\": \"$chatId\",
                      |  \"value\": \"${newMessage.now()}\"
                      |}""".stripMargin

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

    onMountCallback(_ => fetchMessages())

    div(
      h2(s"Chat: $chatId"),
      ul(
        children <-- messages.signal.map(_.map { msg =>
          li(s"${msg.senderId}: ${msg.value}")
        })
      ),
      form(
        onSubmit.preventDefault --> { _ => sendMessage() },
        input(
          controlled(
            value <-- newMessage,
            onInput.mapToValue --> newMessage
          ),
          placeholder := "Type a message...",
          cls := "message-input"
        ),
        button("Send", tpe := "submit")
      ),
      div(cls := "message-status", child.text <-- messageStatus.signal)
    )