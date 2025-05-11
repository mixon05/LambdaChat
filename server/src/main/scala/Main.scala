import zio._
import zio.http.Server
import mongo.MongoModule

object Main extends ZIOAppDefault {
  def run: ZIO[Any, Throwable, Nothing] =
    Server
      .serve(Handlers.routes)
      .provide(
        Server.default,
        MongoModule.live,
      )
}
