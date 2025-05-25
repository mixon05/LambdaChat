import zio._
import zio.http._

object Main extends ZIOAppDefault {
  override def run: ZIO[Any, Throwable, Nothing] = {
    Server
      .serve(Handlers.routes)
      .provide(
        Server.default,
        MongoModule.live
      )
  }
}