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
    def sendMessage(chatId: String, senderId: String, senderUsername: String, value: String): Task[String]
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

    def sendMessage(chatId: String, senderId: String, senderUsername: String, value: String): ZIO[Service, Throwable, String] =
      ZIO.serviceWithZIO[Service](_.sendMessage(chatId, senderId, senderUsername, value))

    def getChatWithMessages(chatId: String): ZIO[Service, Throwable, ChatWithMessages] =
      ZIO.serviceWithZIO[Service](_.getChatWithMessages(chatId))

    def getChat(chatId: String): ZIO[Service, Throwable, Chat] =
      ZIO.serviceWithZIO[Service](_.getChat(chatId))

    def getUsersByIds(userIds: List[String]): ZIO[Service, Throwable, List[User]] =
      ZIO.serviceWithZIO[Service](_.getUsersByIds(userIds))
  }

  private final case class Live(client: MongoClient) extends Service {

    private val DATABASE_NAME = "PF"
    private val USERS_COLLECTION = "users"
    private val CHATS_COLLECTION = "chats"
    private val MESSAGES_COLLECTION = "messages"
    private val DEFAULT_MESSAGE_LIMIT = 50
    private val SEARCH_LIMIT = 20

    private lazy val database = client.getDatabase(DATABASE_NAME)
    private lazy val usersCollection: MongoCollection[Document] =
      database.getCollection(USERS_COLLECTION)
    private lazy val chatsCollection: MongoCollection[Document] =
      database.getCollection(CHATS_COLLECTION)
    private lazy val messagesCollection: MongoCollection[Document] =
      database.getCollection(MESSAGES_COLLECTION)

    private def documentToUser(doc: Document): User =
      User(
        id = doc.getObjectId("_id").toString,
        username = doc.getString("name")
      )

    private def documentToMessage(doc: Document): Message =
      val senderUsername = Option(doc.getString("senderUsername")).getOrElse("") // Bezpieczne pobranie
      Message(
        id = doc.getObjectId("_id").toString,
        chatId = doc.getString("chatId"),
        senderId = doc.getString("senderId"),
        senderUsername = senderUsername,
        value = doc.getString("value"),
        timestamp = doc.getLong("timestamp")
      )

    private def createUserDocument(name: String, passwordHash: String): Document =
      new Document("name", name).append("password_hash", passwordHash)

    private def currentTimeMillis: Long = java.lang.System.currentTimeMillis()

    private def userExists(name: String): Task[Boolean] =
      ZIO.attempt {
        val query = new Document("name", name)
        usersCollection.find(query).first() != null
      }

    def registerUser(name: String, passwordHash: String): Task[Option[String]] =
      userExists(name).flatMap {
        case true => ZIO.succeed(None)
        case false =>
          val doc = createUserDocument(name, passwordHash)
          insertDocumentAndGetId(usersCollection)(doc).map(Some(_))
      }

    private def findUserByCredentials(name: String, passwordHash: String): Task[Option[Document]] =
      ZIO.attempt {
        val query = new Document("name", name).append("password_hash", passwordHash)
        Option(usersCollection.find(query).first())
      }

    def loginUser(name: String, passwordHash: String): Task[Option[User]] =
      findUserByCredentials(name, passwordHash).map(_.map(documentToUser))

    private def createSearchPattern(query: String): Pattern =
      Pattern.compile(Pattern.quote(query), Pattern.CASE_INSENSITIVE)

    def searchUsers(query: String): Task[List[User]] =
      ZIO.attempt {
        val filter = Filters.regex("name", createSearchPattern(query))

        usersCollection.find(filter)
          .limit(SEARCH_LIMIT)
          .asScala
          .map(documentToUser)
          .toList
      }

    private def buildChatPipelineWithUsers(filter: org.bson.conversions.Bson): java.util.List[org.bson.conversions.Bson] =
      List(
        Aggregates.`match`(filter),
        Aggregates.lookup(
          USERS_COLLECTION,
          "userIds",
          "_id",
          "users"
        )
      ).asJava

    private def parseChatAggregationResult(doc: Document): Chat = {
      val userIds = doc.getList("userIds", classOf[String]).asScala.toList
      val userNames = Option(doc.getList("userNames", classOf[String]))
        .map(_.asScala.toList)
        .getOrElse(Nil)
      val userDocs = Option(doc.getList("users", classOf[Document])).map(_.asScala.toList).getOrElse(Nil)
      val users = userDocs.map(documentToUser)

      val finalUserNames = if (userNames.nonEmpty) userNames else users.map(_.username)

      Chat(
        id = doc.getObjectId("_id").toString,
        userIds = userIds,
        userNames = finalUserNames,
        users = users
      )
    }

    def getUserChats(userId: String): Task[List[Chat]] =
      ZIO.attempt {
        val pipeline = buildChatPipelineWithUsers(Filters.in("userIds", userId))

        chatsCollection.aggregate(pipeline)
          .asScala
          .map(parseChatAggregationResult)
          .toList
      }

    def getChatMessages(chatId: String, limit: Int, offset: Int): Task[List[Message]] =
      ZIO.attempt {
        val pipeline = List(
          Aggregates.`match`(Filters.eq("chatId", chatId)),
          Aggregates.sort(Sorts.descending("timestamp")),
          Aggregates.skip(offset),
          Aggregates.limit(limit),
          Aggregates.lookup(
            USERS_COLLECTION,
            "senderId",
            "_id",
            "senderInfo"
          )
        ).asJava

        messagesCollection.aggregate(pipeline)
          .asScala
          .map { doc =>
            val senderDocs = doc.getList("senderInfo", classOf[Document]).asScala
            val sender = senderDocs.headOption.map(documentToUser)

            val finalSenderUsername = sender.map(_.username)
              .orElse(Option(doc.getString("senderUsername")))
              .getOrElse("NN")

            Message(
              id = doc.getObjectId("_id").toString,
              chatId = doc.getString("chatId"),
              senderId = doc.getString("senderId"),
              senderUsername = finalSenderUsername,
              value = doc.getString("value"),
              timestamp = doc.getLong("timestamp"),
              sender = sender
            )
          }
          .toList
          .reverse
      }

    private def insertDocumentAndGetId(collection: MongoCollection[Document])(doc: Document): Task[String] =
      ZIO.attempt {
        collection.insertOne(doc)
        doc.getObjectId("_id").toString
      }

    def createChat(userIds: List[String]): Task[String] = {
      for {
        users <- getUsersByIds(userIds)
        userNames = users.map(_.username)
        doc = new Document()
          .append("userIds", userIds.asJava)
          .append("userNames", userNames.asJava)
          .append("createdAt", currentTimeMillis)
        id <- insertDocumentAndGetId(chatsCollection)(doc)
      } yield id
    }

    def sendMessage(chatId: String, senderId: String, senderUsername: String, value: String): Task[String] = {
      val doc = new Document()
        .append("chatId", chatId)
        .append("senderId", senderId)
        .append("senderUsername", senderUsername)
        .append("value", value)
        .append("timestamp", currentTimeMillis)

      insertDocumentAndGetId(messagesCollection)(doc)
    }

    def getUsersByIds(userIds: List[String]): Task[List[User]] =
      ZIO.when(userIds.nonEmpty) {
        ZIO.attempt {
          val objectIds = userIds.map(new ObjectId(_))
          val filter = Filters.in("_id", objectIds.asJava)

          usersCollection.find(filter)
            .asScala
            .map(documentToUser)
            .toList
        }
      }.map(_.getOrElse(List.empty))

    def getChat(chatId: String): Task[Chat] = {
      val tryGetChat = for {
        objectId <- ZIO.attempt(new ObjectId(chatId))
          .mapError(e => new IllegalArgumentException(s"Invalid chat ID format: $chatId", e))

        pipeline = buildChatPipelineWithUsers(Filters.eq("_id", objectId))

        result <- ZIO.attempt(Option(chatsCollection.aggregate(pipeline).first()))

        chat <- result match {
          case Some(doc) => ZIO.succeed(parseChatAggregationResult(doc))
          case None => ZIO.fail(new NoSuchElementException(s"Chat with ID $chatId not found"))
        }
      } yield chat

      tryGetChat
    }

    def getChatWithMessages(chatId: String): Task[ChatWithMessages] =
      (getChat(chatId) <&> getChatMessages(chatId, DEFAULT_MESSAGE_LIMIT, 0))
        .map { case (chat, messages) => ChatWithMessages(chat, messages) }
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