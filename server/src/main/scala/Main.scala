import zio._
import zio.http._
import zio.http.ChannelEvent.{ExceptionCaught, Read, UserEvent, UserEventTriggered}

object Main extends ZIOAppDefault {

  private val routes =
    Routes(
      Method.POST / "register" -> handler { (req: Request) =>
        Response.text("User registered (implement me)")
      },
      Method.POST / "login" -> handler { (req: Request) =>
        Response.text("User logged in (implement me)")
      },
      Method.GET / "search" / string("username") -> handler { (username: String, req: Request) =>
        Response.text(s"Searching for user: $username (implement me)")
      },
      Method.GET / "chat" / string("userId") -> handler { (userId: String, req: Request) =>
        Response.text("User logged in (implement me)")
      },
    )




  def run: ZIO[Any, Throwable, Nothing] = Server.serve(routes).provide(Server.default)
}