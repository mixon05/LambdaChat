import com.mongodb.{ConnectionString, MongoClientSettings, ServerApi, ServerApiVersion}
import com.mongodb.client.{MongoClient, MongoClients, MongoCollection}
import com.mongodb.client.model.{Aggregates, Filters, Sorts}
import org.bson.Document
import org.bson.types.ObjectId
import zio._
import com.typesafe.config.ConfigFactory
import scala.jdk.CollectionConverters._
import java.util.regex.Pattern

object MongoModule {

  trait Service {
    def client: MongoClient
    def registerUser(name: String, passwordHash: String): Task[Option[String]]
    def loginUser(name: String, passwordHash: String): Task[Option[User]]
    def searchUsers(query: String): Task[List[User]]
    def getUserChats(userId: String): Task[List[Chat]]
    def getChatMessages(chatId: String, limit: Int, offset: Int): Task[List[Message]]
    def createChat(userIds: List[String]): Task[String]
    def sendMessage(chatId: String, senderId: String, value: String): Task[String]
    def getChatWithMessages(chatId: String): Task[ChatWithMessages]
    def getChat(chatId: String): Task[Chat]
    def getUsersByIds(userIds: List[String]): Task[List[User]]
  }

  object Service {
    def client: URIO[Service, MongoClient] =
      ZIO.serviceWith[Service](_.client)

    def registerUser(name: String, passwordHash: String): ZIO[Service, Throwable, Option[String]] =
      ZIO.serviceWithZIO[Service](_.registerUser(name, passwordHash))

    def loginUser(name: String, passwordHash: String): ZIO[Service, Throwable, Option[User]] =
      ZIO.serviceWithZIO[Service](_.loginUser(name, passwordHash))

    def searchUsers(query: String): ZIO[Service, Throwable, List[User]] =
      ZIO.serviceWithZIO[Service](_.searchUsers(query))

    def getUserChats(userId: String): ZIO[Service, Throwable, List[Chat]] =
      ZIO.serviceWithZIO[Service](_.getUserChats(userId))

    def getChatMessages(chatId: String, limit: Int, offset: Int): ZIO[Service, Throwable, List[Message]] =
      ZIO.serviceWithZIO[Service](_.getChatMessages(chatId, limit, offset))

    def createChat(userIds: List[String]): ZIO[Service, Throwable, String] =
      ZIO.serviceWithZIO[Service](_.createChat(userIds))

    def sendMessage(chatId: String, senderId: String, value: String): ZIO[Service, Throwable, String] =
      ZIO.serviceWithZIO[Service](_.sendMessage(chatId, senderId, value))

    def getChatWithMessages(chatId: String): ZIO[Service, Throwable, ChatWithMessages] =
      ZIO.serviceWithZIO[Service](_.getChatWithMessages(chatId))

    def getChat(chatId: String): ZIO[Service, Throwable, Chat] =
      ZIO.serviceWithZIO[Service](_.getChat(chatId))

    def getUsersByIds(userIds: List[String]): ZIO[Service, Throwable, List[User]] =
      ZIO.serviceWithZIO[Service](_.getUsersByIds(userIds))
  }

  private final case class Live(client: MongoClient) extends Service {

    private def usersCollection: MongoCollection[Document] =
      client.getDatabase("PF").getCollection("users")

    private def chatsCollection: MongoCollection[Document] =
      client.getDatabase("PF").getCollection("chats")

    private def messagesCollection: MongoCollection[Document] =
      client.getDatabase("PF").getCollection("messages")

    def registerUser(name: String, passwordHash: String): Task[Option[String]] = ZIO.attempt {
      val query = new Document("name", name)
      val existing = usersCollection.find(query).first()
      if (existing != null) {
        None
      } else {
        val doc = new Document("name", name).append("password_hash", passwordHash)
        usersCollection.insertOne(doc)
        Some(doc.getObjectId("_id").toString)
      }
    }

    def loginUser(name: String, passwordHash: String): Task[Option[User]] = ZIO.attempt {
      val query = new Document("name", name).append("password_hash", passwordHash)
      val result = usersCollection.find(query).first()
      if (result != null) {
        Some(User(
          id = result.getObjectId("_id").toString,
          username = result.getString("name")
        ))
      } else {
        None
      }
    }

    def searchUsers(query: String): Task[List[User]] = ZIO.attempt {
      val pattern = Pattern.compile(Pattern.quote(query), Pattern.CASE_INSENSITIVE)
      val filter = Filters.regex("name", pattern)

      usersCollection.find(filter)
        .limit(20)
        .asScala
        .map { doc =>
          User(
            id = doc.getObjectId("_id").toString,
            username = doc.getString("name")
          )
        }
        .toList
    }

    def getUserChats(userId: String): Task[List[Chat]] = ZIO.attempt {
      val pipeline = List(
        Aggregates.`match`(Filters.in("userIds", userId)),

        Aggregates.lookup(
          "users",
          "userIds",
          "_id",
          "users"
        )
      ).asJava

      chatsCollection.aggregate(pipeline)
        .asScala
        .map { doc =>
          val userIds = doc.getList("userIds", classOf[String]).asScala.toList
          val userDocs = doc.getList("users", classOf[Document]).asScala.toList

          val users = userDocs.map { userDoc =>
            User(
              id = userDoc.getObjectId("_id").toString,
              username = userDoc.getString("name")
            )
          }

          Chat(
            id = doc.getObjectId("_id").toString,
            userIds = userIds,
            users = users
          )
        }
        .toList
    }

    def getChatMessages(chatId: String, limit: Int, offset: Int): Task[List[Message]] = ZIO.attempt {
      val filter = Filters.eq("chatId", chatId)

      messagesCollection.find(filter)
        .sort(Sorts.descending("timestamp"))
        .skip(offset)
        .limit(limit)
        .asScala
        .map { doc =>
          Message(
            id = doc.getObjectId("_id").toString,
            chatId = doc.getString("chatId"),
            senderId = doc.getString("senderId"),
            value = doc.getString("value"),
            timestamp = doc.getLong("timestamp")
          )
        }
        .toList
        .reverse // chronological order
    }

    def createChat(userIds: List[String]): Task[String] = ZIO.attempt {
      val doc = new Document()
        .append("userIds", userIds.asJava)
        .append("createdAt", java.lang.System.currentTimeMillis())

      chatsCollection.insertOne(doc)
      doc.getObjectId("_id").toString
    }

    def sendMessage(chatId: String, senderId: String, value: String): Task[String] = ZIO.attempt {
      val doc = new Document()
        .append("chatId", chatId)
        .append("senderId", senderId)
        .append("value", value)
        .append("timestamp", java.lang.System.currentTimeMillis())

      messagesCollection.insertOne(doc)
      doc.getObjectId("_id").toString
    }

    def getUsersByIds(userIds: List[String]): Task[List[User]] = ZIO.attempt {
      if (userIds.isEmpty) {
        List.empty
      } else {
        val objectIds = userIds.map(new ObjectId(_))
        val filter = Filters.in("_id", objectIds.asJava)

        usersCollection.find(filter)
          .asScala
          .map { doc =>
            User(
              id = doc.getObjectId("_id").toString,
              username = doc.getString("name")
            )
          }
          .toList
      }
    }

    def getChat(chatId: String): Task[Chat] = ZIO.attempt {
      val pipeline = List(
        Aggregates.`match`(Filters.eq("_id", new ObjectId(chatId))),

        Aggregates.lookup(
          "users",
          "userIds",
          "_id",
          "users"
        )
      ).asJava

      val result = chatsCollection.aggregate(pipeline).first()

      if (result != null) {
        val userIds = result.getList("userIds", classOf[String]).asScala.toList
        val userDocs = result.getList("users", classOf[Document]).asScala.toList

        val users = userDocs.map { userDoc =>
          User(
            id = userDoc.getObjectId("_id").toString,
            username = userDoc.getString("name")
          )
        }

        Chat(
          id = result.getObjectId("_id").toString,
          userIds = userIds,
          users = users
        )
      } else {
        throw new Exception("Chat not found")
      }
    }

    def getChatWithMessages(chatId: String): Task[ChatWithMessages] = for {
      chat <- getChat(chatId)
      messages <- getChatMessages(chatId, 50, 0)
    } yield ChatWithMessages(chat, messages)
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