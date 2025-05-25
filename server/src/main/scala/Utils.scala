import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim}
import java.time.Clock
import scala.util.{Success, Failure}
import zio.json._
import com.typesafe.config.ConfigFactory

object Utils {
  import java.security.MessageDigest

  def hash(password: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val bytes = md.digest(password.getBytes("UTF-8"))
    bytes.map("%02x".format(_)).mkString
  }

  // JWT configuration
  private val config = ConfigFactory.load()
  private val secretKey = config.getString("app.jwt.SECRET_KEY")
  private val algorithm = JwtAlgorithm.HS256
  implicit val clock: Clock = Clock.systemUTC

  case class TokenPayload(userId: String, username: String)
  private object TokenPayload {
    implicit val codec: JsonCodec[TokenPayload] = DeriveJsonCodec.gen[TokenPayload]
  }

  def generateToken(userId: String, username: String): String = {
    val payload = TokenPayload(userId, username)
    val claim = JwtClaim(
      content = payload.toJson,
      expiration = Some(java.lang.System.currentTimeMillis() / 1000 + 86400), // 24 hours
      issuedAt = Some(java.lang.System.currentTimeMillis() / 1000)
    )

    Jwt.encode(claim, secretKey, algorithm)
  }

  def verifyToken(token: String): Option[(String, String)] = {
    Jwt.decode(token, secretKey, Seq(algorithm)) match {
      case Success(claim) =>
        claim.content.fromJson[TokenPayload] match {
          case Right(payload) =>
            claim.expiration match {
              case Some(exp) if exp * 1000 > java.lang.System.currentTimeMillis() =>
                Some((payload.userId, payload.username))
              case _ => None
            }
          case Left(_) => None
        }
      case Failure(_) => None
    }
  }
}
