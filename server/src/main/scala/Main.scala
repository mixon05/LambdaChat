import zio._
import zio.http._
import mongo.MongoModule

object Main extends ZIOAppDefault {
  def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    Server.serve(
      Middleware.cors(Handlers.routes)
    ).provide(
      Server.default,
      MongoModule.live,
    )
  }
}
