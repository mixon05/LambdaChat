import zio.json.{DeriveJsonCodec, JsonCodec}

case class UserRegistration(username: String, password: String)
object UserRegistration {
  implicit val codec: JsonCodec[UserRegistration] = DeriveJsonCodec.gen[UserRegistration]
}
