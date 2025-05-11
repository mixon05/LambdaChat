package mongo

import com.mongodb.ConnectionString
import com.mongodb.MongoClientSettings
import com.mongodb.ServerApi
import com.mongodb.ServerApiVersion
import com.mongodb.client.{MongoClient, MongoClients}
import zio._

object MongoModule {
  trait Service {
    def client: MongoClient
  }

  object Service {
    def client: URIO[MongoModule, MongoClient] =
      ZIO.serviceWith[Service](_.client)
  }

  private case class Live(client: MongoClient) extends Service

  val live: ZLayer[Any, Throwable, MongoModule] =
    ZLayer.scoped {
      for {
        uri <- ZIO
          .fromOption(Option(System.getenv("MONGODB_URI")))
          .orElseFail(new RuntimeException("MONGODB_URI not set"))
        settings = MongoClientSettings.builder()
          .applyConnectionString(new ConnectionString(uri))
          .serverApi(ServerApi.builder().version(ServerApiVersion.V1).build())
          .build()
        client  <- ZIO.acquireRelease(
          ZIO.attempt(MongoClients.create(settings))
        )(c => ZIO.attempt(c.close()).orDie)
      } yield Live(client)
    }
}
