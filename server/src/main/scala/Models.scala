import zio.json._

case class UserRegistration(username: String, password: String)
object UserRegistration {
  implicit val codec: JsonCodec[UserRegistration] = DeriveJsonCodec.gen[UserRegistration]
}

case class User(id: String, username: String)
object User {
  implicit val codec: JsonCodec[User] = DeriveJsonCodec.gen[User]
}

case class AuthResponse(token: String, userId: String, username: String)
object AuthResponse {
  implicit val codec: JsonCodec[AuthResponse] = DeriveJsonCodec.gen[AuthResponse]
}

case class Message(
                    id: String,
                    chatId: String,
                    senderId: String,
                    senderUsername: String,
                    value: String,
                    timestamp: Long,
                    sender: Option[User] = None
                  )
object Message {
  implicit val codec: JsonCodec[Message] = DeriveJsonCodec.gen[Message]
}

case class Chat(
                 id: String,
                 userIds: List[String],
                 userNames: List[String],
                 users: List[User]
               )
object Chat {
  implicit val codec: JsonCodec[Chat] = DeriveJsonCodec.gen[Chat]
}

case class ChatWithMessages(
                             chat: Chat,
                             messages: List[Message]
                           )
object ChatWithMessages {
  implicit val codec: JsonCodec[ChatWithMessages] = DeriveJsonCodec.gen[ChatWithMessages]
}

case class CreateChatRequest(userIds: List[String])
object CreateChatRequest {
  implicit val codec: JsonCodec[CreateChatRequest] = DeriveJsonCodec.gen[CreateChatRequest]
}

case class SendMessageRequest(chatId: String, value: String)
object SendMessageRequest {
  implicit val codec: JsonCodec[SendMessageRequest] = DeriveJsonCodec.gen[SendMessageRequest]
}