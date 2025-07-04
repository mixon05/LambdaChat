package webapp

import scala.scalajs.js
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
import scala.scalajs.js.timers.setInterval


object ChatPage:

  case class Message(id: String, chatId: String, senderId: String, senderUsername: String, value: String, timestamp: Long)
  object Message:
    implicit val rw: ReadWriter[Message] = macroRW

  case class Chat(id: String, userIds: List[String], userNames: List[String])
  object Chat:
    implicit val rw: ReadWriter[Chat] = macroRW

  val emojiMap: Map[String, String] = Map(
    ":like:" -> "/emojis/like.svg"
  )

  def formatTimestamp(timestamp: Long): String = {
    val date = new js.Date(timestamp.toDouble)

    val hours = date.getHours().toInt
    val minutes = date.getMinutes().toInt

    val formattedHours = f"$hours%02d"
    val formattedMinutes = f"$minutes%02d"

    s"$formattedHours:$formattedMinutes"
  }

  def getAuthTokenAndUserId: Option[(String, String)] =
    Option(dom.window.localStorage.getItem("token"))
      .zip(Option(dom.window.localStorage.getItem("userId")))

  def isOnlyEmoji(text: String): Boolean = {
    val trimmed = text.trim
    emojiMap.contains(trimmed)
  }

  def renderMessage(msg: Message): Element =
    val currentUserId = dom.window.localStorage.getItem("userId")
    val clsName =
      if msg.senderId == currentUserId then "message-item message-right"
      else "message-item message-left"

    val timeStr = formatTimestamp(msg.timestamp)

    if isOnlyEmoji(msg.value) then
      li(
        cls := clsName,
        div(
          cls := "message-content emoji-only",
          emojiMap.get(msg.value.trim).map { emojiPath =>
            img(
              src := emojiPath,
              cls := "emoji",
              alt := msg.value,
              width := "100px",
              height := "100px"
            )
          }.getOrElse(span(msg.value))
        ),
        div(cls := "message-time", timeStr)
      )
    else
      li(
        cls := clsName,
        div(
          cls := "message-content",
          s"${msg.senderUsername}: ${msg.value}"
        ),
        div(cls := "message-time", timeStr)
      )

  def fetchChatInfo(chatId: String, chat: Var[Option[Chat]], messageStatus: Var[String]): Unit =
    getAuthTokenAndUserId.fold {
      messageStatus.set("Not logged in")
    } { case (token, userId) =>
      Ajax.get(
        url = s"http://localhost:8080/users/$userId/chats",
        headers = Map("Authorization" -> s"Bearer $token")
      ).map { xhr =>
        if xhr.status == 200 then
          Try(read[List[Chat]](xhr.responseText)).fold(
            ex => messageStatus.set(s"Chat list decode error: ${ex.getMessage}"),
            chats => {
              chats.find(_.id == chatId) match {
                case Some(foundChat) =>
                  chat.set(Some(foundChat))
                case None =>
                  messageStatus.set("Chat not found in user's chat list")
              }
            }
          )
        else
          messageStatus.set(s"Failed to fetch chat list: ${xhr.status}")
      }.recover {
        case _ => messageStatus.set("Network error during chat fetch")
      }
    }

  def fetchMessages(chatId: String, messages: Var[List[Message]], messageStatus: Var[String], loading: Var[Boolean]): Unit =
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

  def sendMessage(chatId: String, newMessage: Var[String], messageStatus: Var[String], messages: Var[List[Message]], loading: Var[Boolean]): Unit =
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
          fetchMessages(chatId, messages, messageStatus, loading)
        else
          messageStatus.set(s"Error: ${xhr.status}")
      }.recover {
        case _ => messageStatus.set("Network error")
      }
    }

  def sendLike(chatId: String, messageStatus: Var[String], messages: Var[List[Message]], loading: Var[Boolean]): Unit =
    getAuthTokenAndUserId.fold {
      messageStatus.set("Not logged in")
    } { case (token, senderId) =>
      val body = write(Map("chatId" -> chatId, "value" -> ":like:"))

      Ajax.post(
        url = "http://localhost:8080/messages",
        data = body,
        headers = Map(
          "Content-Type" -> "application/json",
          "Authorization" -> s"Bearer $token"
        )
      ).map { xhr =>
        if xhr.status == 200 then
          fetchMessages(chatId, messages, messageStatus, loading)
        else
          messageStatus.set(s"Error: ${xhr.status}")
      }.recover {
        case _ => messageStatus.set("Network error")
      }
    }

  def apply(chatId: String): Element =
    val messages       = Var(List.empty[Message])
    val newMessage     = Var("")
    val messageStatus  = Var("")
    val loading        = Var(false)
    val chat           = Var(Option.empty[Chat])

    div(
      onMountCallback { _ =>
        fetchChatInfo(chatId, chat, messageStatus)
        fetchMessages(chatId, messages, messageStatus, loading)
        setInterval(1000) {
          fetchMessages(chatId, messages, messageStatus, loading)
        }
      },
      cls := "chat-container",
      div(
        cls := "chat-header",
        child.maybe <-- chat.signal.map {
          case Some(ch) if ch.userNames.nonEmpty =>
            Some(h2(s"Chat with: ${ch.userNames.drop(1).mkString(", ")}"))
          case Some(_) =>
            Some(h2("Chat"))
          case None =>
            Some(h2("Loading chat..."))
        },
        child.maybe <-- messageStatus.signal.map {
          case ""  => None
          case msg => Some(div(cls := "chat-error", msg))
        }
      ),
      div(
        cls := "messages-section",
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
        onSubmit.preventDefault --> { _ => sendMessage(chatId, newMessage, messageStatus, messages, loading) },
        button(
          "👍",
          tpe := "button",
          cls := "like-button",
          onClick.preventDefault --> { _ => sendLike(chatId, messageStatus, messages, loading) }
        ),
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