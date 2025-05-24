package mongo

import com.mongodb.{ConnectionString, MongoClientSettings, ServerApi, ServerApiVersion}
import com.mongodb.client.{MongoClient, MongoClients, MongoCollection}
import org.bson.Document
import zio._
import com.typesafe.config.ConfigFactory

object MongoModule {

  trait Service {
    def client: MongoClient
    def registerUser(name: String, passwordHash: String): Task[Boolean]
    def loginUser(name: String, passwordHash: String): Task[Boolean]
  }

  object Service {
    def client: URIO[Service, MongoClient] =
      ZIO.serviceWith[Service](_.client)

    def registerUser(name: String, passwordHash: String): ZIO[Service, Throwable, Boolean] =
      ZIO.serviceWithZIO[Service](_.registerUser(name, passwordHash))

    def loginUser(name: String, passwordHash: String): ZIO[Service, Throwable, Boolean] =
      ZIO.serviceWithZIO[Service](_.loginUser(name, passwordHash))
  }

  private final case class Live(client: MongoClient) extends Service {

    private def usersCollection: MongoCollection[Document] =
      client.getDatabase("PF").getCollection("users")

    def registerUser(name: String, passwordHash: String): Task[Boolean] = ZIO.attempt {
      val query = new Document("name", name)
      val existing = usersCollection.find(query).first()
      if (existing != null) {
        false
      } else {
        val doc = new Document("name", name).append("password_hash", passwordHash)
        usersCollection.insertOne(doc)
        true
      }
    }

    def loginUser(name: String, passwordHash: String): Task[Boolean] = ZIO.attempt {
      val query = new Document("name", name).append("password_hash", passwordHash)
      val result = usersCollection.find(query).first()
      result != null
    }
  }

  private val config = ConfigFactory.load()

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
