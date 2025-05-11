package mongo

import com.mongodb.{ConnectionString, MongoClientSettings, ServerApi, ServerApiVersion}
import com.mongodb.client.{MongoClient, MongoClients}
import zio._
import com.typesafe.config.ConfigFactory

object MongoModule {

  trait Service {
    def client: MongoClient
  }

  object Service {
    def client: URIO[Service, MongoClient] =
      ZIO.serviceWith[Service](_.client)
  }

  private final case class Live(client: MongoClient) extends Service

  val config = ConfigFactory.load()

  val live: ZLayer[Any, Throwable, Service] = ZLayer.scoped {
    for {
      uri <- ZIO.attempt(config.getString("app.db.MONGO_URL"))
        .orElseFail(new RuntimeException("MONGO_URL not set"))

      settings = MongoClientSettings.builder()
        .applyConnectionString(new ConnectionString(uri))
        .serverApi(ServerApi.builder().version(ServerApiVersion.V1).build())
        .build()

      client <- ZIO.acquireRelease(
        ZIO.attempt(MongoClients.create(settings))
      )(c => ZIO.attempt(c.close()).orDie)

    } yield Live(client)
  }
}