import zio._
import zio.http._
import zio.json._
import mongo.MongoModule

object Handlers {
  val routes: Routes[MongoModule.Service, Response] = Routes(
    Method.GET / "hello" ->
      handler(Response.text("Hello, World!")),

    Method.POST / "login" -> handler { (req: Request) =>
      (for {
        body       <- req.body.asString
        userLogin  <- ZIO.fromEither(body.fromJson[UserRegistration])
        hashedPassword = Utils.hash(userLogin.password)
        success    <- MongoModule.Service.loginUser(userLogin.username, hashedPassword)
      } yield if (success) Response.status(Status.Ok) else Response.status(Status.Unauthorized))
        .catchAll { error =>
          ZIO.succeed(Response.status(Status.BadRequest))
        }
    },

    Method.POST / "register" -> handler { (req: Request) =>
    (for {
      body       <- req.body.asString
      userLogin  <- ZIO.fromEither(body.fromJson[UserRegistration])
      hashedPassword = Utils.hash(userLogin.password)
      success    <- MongoModule.Service.registerUser(userLogin.username, hashedPassword)
    } yield if (success) Response.status(Status.Ok) else Response.status(Status.Unauthorized))
      .catchAll { error =>
        ZIO.succeed(Response.status(Status.BadRequest))
      }
    },
  )
}