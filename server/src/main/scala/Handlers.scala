import zio._
import zio.http._
import zio.json._

object Handlers {
  private def extractBearerToken(req: Request): Option[String] = {
    req.headers.get(Header.Authorization).flatMap { authHeader =>
      val headerValue = authHeader.renderedValue
      if (headerValue.startsWith("Bearer ")) Some(headerValue.substring(7))
      else None
    }
  }

  private def withAuth[R, E](req: Request)(
    action: (String, String) => ZIO[R, E, Response]
  ): ZIO[R, E, Response] = {
    extractBearerToken(req).flatMap(Utils.verifyToken) match {
      case Some((userId, username)) => action(userId, username)
      case None => ZIO.succeed(Response.text("Unauthorized").status(Status.Unauthorized))
    }
  }

  private def parseBody[A: JsonDecoder](req: Request): ZIO[Any, Throwable, A] = {
    for {
      body <- req.body.asString
      parsed <- ZIO.fromEither(body.fromJson[A]).mapError(new Exception(_))
    } yield parsed
  }

  private def checkChatAccess(chat: Chat, userId: String): ZIO[Any, Throwable, Unit] = {
    if (chat.userIds.contains(userId)) ZIO.unit
    else ZIO.fail(new Exception("Forbidden"))
  }

  private def handleError(error: Throwable): Response =
    Response.text(error.getMessage).status(Status.BadRequest)

  private def handleForbiddenError(error: Throwable): Response =
    Response.text("Forbidden").status(Status.Forbidden)

  private def handleNotFoundError(error: Throwable): Response =
    Response.status(Status.NotFound)

  private def getQueryParam(req: Request, name: String): Option[String] =
    req.url.queryParams.getAll(name).headOption

  private def getIntQueryParam(req: Request, name: String, default: Int): Int =
    getQueryParam(req, name).flatMap(_.toIntOption).getOrElse(default)

  private def jsonResponse[A: JsonEncoder](data: A): ZIO[Any, Throwable, Response] =
    ZIO.attempt(Response.json(data.toJson))

  private def createAuthResponse(userId: String, username: String): ZIO[Any, Throwable, Response] = {
    val token = Utils.generateToken(userId, username)
    jsonResponse(AuthResponse(token, userId, username))
  }

  private def checkUserAccess(requestedUserId: String, authUserId: String): ZIO[Any, Throwable, Unit] = {
    if (requestedUserId == authUserId) ZIO.unit
    else ZIO.fail(new Exception("Forbidden"))
  }

  val routes: Routes[MongoModule.Service, Response] = Routes(
    Method.GET / "hello" ->
      handler(Response.text("Hello, World!")),

    Method.POST / "login" -> Handler.fromFunctionZIO[Request] {  req =>
      (for {
        userLogin  <- parseBody[UserRegistration](req)
        hashedPassword = Utils.hash(userLogin.password)
        mongo      <- ZIO.service[MongoModule.Service]
        userOpt    <- mongo.loginUser(userLogin.username, hashedPassword)
        response   <- userOpt match {
          case Some(user) => createAuthResponse(user.id, user.username)
          case None => ZIO.succeed(Response.text("Invalid credentials").status(Status.Unauthorized))
        }
      } yield response)
        .catchAll(error => ZIO.succeed(Response.status(Status.BadRequest)))
    }.mapError(_ => Response.status(Status.BadRequest)),

    Method.POST / "register" -> Handler.fromFunctionZIO[Request] {  req =>
      (for {
        userLogin  <- parseBody[UserRegistration](req)
        hashedPassword = Utils.hash(userLogin.password)
        userIdOpt  <- MongoModule.Service.registerUser(userLogin.username, hashedPassword)
        response   <- userIdOpt match {
          case Some(userId) => createAuthResponse(userId, userLogin.username)
          case None => ZIO.succeed(Response.text("User already exists").status(Status.Conflict))
        }
      } yield response)
        .catchAll(error => ZIO.succeed(Response.status(Status.BadRequest)))
    }.mapError(_ => Response.status(Status.BadRequest)),

    Method.GET / "users" / "search" -> handler { (req: Request) =>
      withAuth(req) { (userId, username) =>
        (for {
          query <- ZIO.fromOption(getQueryParam(req, "q"))
            .orElseFail(new Exception("Query parameter 'q' is required"))
          users <- MongoModule.Service.searchUsers(query)
          response <- jsonResponse(users)
        } yield response)
          .catchAll(error => ZIO.succeed(handleError(error)))
      }
    },

    Method.GET / "users" / string("userId") / "chats" ->
      handler { (userId: String, req: Request) =>
        withAuth(req) { (authUserId, authUsername) =>
          (for {
            _ <- checkUserAccess(userId, authUserId)
            chats <- MongoModule.Service.getUserChats(userId)
            response <- jsonResponse(chats)
          } yield response)
            .catchAll(error => ZIO.succeed(handleForbiddenError(error)))
        }
      },

    Method.GET / "chats" / string("chatId") / "messages" ->
      handler { (chatId: String, req: Request) =>
        withAuth(req) { (userId, username) =>
          (for {
            chat <- MongoModule.Service.getChat(chatId)
            _ <- checkChatAccess(chat, userId)
            limit = getIntQueryParam(req, "limit", 50)
            offset = getIntQueryParam(req, "offset", 0)
            messages <- MongoModule.Service.getChatMessages(chatId, limit, offset)
            response <- jsonResponse(messages)
          } yield response)
            .catchAll(error => ZIO.succeed(handleForbiddenError(error)))
        }
      },

    Method.POST / "chats" -> handler { (req: Request) =>
      withAuth(req) { (userId, username) =>
        (for {
          request <- parseBody[CreateChatRequest](req)
          userIds = if (request.userIds.contains(userId)) request.userIds else userId :: request.userIds
          chatId  <- MongoModule.Service.createChat(userIds)
          response <- jsonResponse(Map("chatId" -> chatId))
        } yield response)
          .catchAll(error => ZIO.succeed(Response.status(Status.BadRequest)))
      }
    },

    Method.POST / "messages" -> handler { (req: Request) =>
      withAuth(req) { (userId, username) =>
        (for {
          request <- parseBody[SendMessageRequest](req)
          chat <- MongoModule.Service.getChat(request.chatId)
          _ <- checkChatAccess(chat, userId)
          messageId <- MongoModule.Service.sendMessage(request.chatId, userId, request.value)
          response <- jsonResponse(Map("messageId" -> messageId))
        } yield response)
          .catchAll(error => ZIO.succeed(handleForbiddenError(error)))
      }
    },

    Method.GET / "chats" / string("chatId") ->
      handler { (chatId: String, req: Request) =>
        withAuth(req) { (userId, username) =>
          (for {
            chatWithMessages <- MongoModule.Service.getChatWithMessages(chatId)
            _ <- checkChatAccess(chatWithMessages.chat, userId)
            response <- jsonResponse(chatWithMessages)
          } yield response)
            .catchAll(error => ZIO.succeed(handleNotFoundError(error)))
        }
      }
  )
}