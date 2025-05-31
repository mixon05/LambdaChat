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

  val routes: Routes[MongoModule.Service, Response] = Routes(
    Method.GET / "hello" ->
      handler(Response.text("Hello, World!")),

    Method.POST / "login" -> Handler.fromFunctionZIO[Request] {  req =>
      (for {
        body       <- req.body.asString
        userLogin  <- ZIO.fromEither(body.fromJson[UserRegistration])
        hashedPassword = Utils.hash(userLogin.password)
        mongo      <- ZIO.service[MongoModule.Service]
        userOpt    <- mongo.loginUser(userLogin.username, hashedPassword)
        response   <- userOpt match {
          case Some(user) =>
            val token = Utils.generateToken(user.id, user.username)
            ZIO.succeed(Response.json(AuthResponse(token, user.id, user.username).toJson))
          case None =>
            ZIO.succeed(Response.text("Invalid credentials").status(Status.Unauthorized))
        }
      } yield response)
        .catchAll { error =>
            ZIO.succeed(Response.status(Status.BadRequest))
        }
    }.mapError(_ => Response.status(Status.BadRequest)),

    Method.POST / "register" -> Handler.fromFunctionZIO[Request] {  req =>
      (for {
        body       <- req.body.asString
        userLogin  <- ZIO.fromEither(body.fromJson[UserRegistration])
        userPassword = userLogin.password
        hashedPassword = Utils.hash(userLogin.password)
        userIdOpt  <- MongoModule.Service.registerUser(userLogin.username, hashedPassword)
        response   <- userIdOpt match {
          case Some(userId) =>
            val token = Utils.generateToken(userId, userLogin.username)
            ZIO.succeed(Response.json(AuthResponse(token, userId, userLogin.username).toJson))
          case None =>
            ZIO.succeed(Response.text("User already exists").status(Status.Conflict))
        }
      } yield response).catchAll { error =>
          ZIO.succeed(Response.status(Status.BadRequest))
      }
    }.mapError(_ => Response.status(Status.BadRequest)),

    Method.GET / "users" / "search" -> handler { (req: Request) =>
      extractBearerToken(req).flatMap(Utils.verifyToken) match {
        case Some((userId, username)) =>
          (for {
            query <- ZIO.fromOption(req.url.queryParams.getAll("q").headOption)
              .orElseFail(new Exception("Query parameter 'q' is required"))
            users <- MongoModule.Service.searchUsers(query)
            json  <- ZIO.attempt(users.toJson)
          } yield Response.json(json))
            .catchAll { error =>
              ZIO.succeed(Response.text(error.getMessage).status(Status.BadRequest))
            }
        case None =>
          ZIO.succeed(Response.text("Unauthorized").status(Status.Unauthorized))
      }
    },

    Method.GET / "users" / string("userId") / "chats" ->
      handler { (userId: String, req: Request) =>
        extractBearerToken(req).flatMap(Utils.verifyToken) match {
          case Some((authUserId, authUsername)) =>
            if (userId != authUserId) {
              ZIO.succeed(Response.text("Forbidden").status(Status.Forbidden))
            } else {
              (for {
                chats <- MongoModule.Service.getUserChats(userId)
                json  <- ZIO.attempt(chats.toJson)
              } yield Response.json(json))
                .catchAll { error =>
                  ZIO.succeed(Response.status(Status.InternalServerError))
                }
            }
          case None =>
            ZIO.succeed(Response.text("Unauthorized").status(Status.Unauthorized))
        }
      },

    Method.GET / "chats" / string("chatId") / "messages" ->
      handler { (chatId: String, req: Request) =>
        extractBearerToken(req).flatMap(Utils.verifyToken) match {
          case Some((userId, username)) =>
            (for {
              chat <- MongoModule.Service.getChat(chatId)
              _ <- if (chat.userIds.contains(userId)) ZIO.unit
              else ZIO.fail(new Exception("Forbidden"))
              limit  <- ZIO.succeed(req.url.queryParams.getAll("limit").headOption.flatMap(_.toIntOption).getOrElse(50))
              offset <- ZIO.succeed(req.url.queryParams.getAll("offset").headOption.flatMap(_.toIntOption).getOrElse(0))
              messages <- MongoModule.Service.getChatMessages(chatId, limit, offset)
              json     <- ZIO.attempt(messages.toJson)
            } yield Response.json(json))
              .catchAll {
                case _: Exception => ZIO.succeed(Response.text("Forbidden").status(Status.Forbidden))
                case _ => ZIO.succeed(Response.status(Status.InternalServerError))
              }
          case None =>
            ZIO.succeed(Response.text("Unauthorized").status(Status.Unauthorized))
        }
      },

    Method.POST / "chats" -> handler { (req: Request) =>
      extractBearerToken(req).flatMap(Utils.verifyToken) match {
        case Some((userId, username)) =>
          (for {
            body    <- req.body.asString
            request <- ZIO.fromEither(body.fromJson[CreateChatRequest])
            userIds = if (request.userIds.contains(userId)) request.userIds else userId :: request.userIds
            chatId  <- MongoModule.Service.createChat(userIds)
            json    <- ZIO.attempt(Map("chatId" -> chatId).toJson)
          } yield Response.json(json))
            .catchAll { error =>
              ZIO.succeed(Response.status(Status.BadRequest))
            }
        case None =>
          ZIO.succeed(Response.text("Unauthorized").status(Status.Unauthorized))
      }
    },

    Method.POST / "messages" -> handler { (req: Request) =>
      extractBearerToken(req).flatMap(Utils.verifyToken) match {
        case Some((userId, username)) =>
          (for {
            body    <- req.body.asString
            request <- ZIO.fromEither(body.fromJson[SendMessageRequest])
            chat <- MongoModule.Service.getChat(request.chatId)
            _ <- if (chat.userIds.contains(userId)) ZIO.unit
            else ZIO.fail(new Exception("Forbidden"))
            messageId <- MongoModule.Service.sendMessage(request.chatId, userId, request.value)
            json      <- ZIO.attempt(Map("messageId" -> messageId).toJson)
          } yield Response.json(json))
            .catchAll {
              case _: Exception => ZIO.succeed(Response.text("Forbidden").status(Status.Forbidden))
              case _ => ZIO.succeed(Response.status(Status.BadRequest))
            }
        case None =>
          ZIO.succeed(Response.text("Unauthorized").status(Status.Unauthorized))
      }
    },

    Method.GET / "chats" / string("chatId") ->
      handler { (chatId: String, req: Request) =>
        extractBearerToken(req).flatMap(Utils.verifyToken) match {
          case Some((userId, username)) =>
            (for {
              chatWithMessages <- MongoModule.Service.getChatWithMessages(chatId)
              _ <- if (chatWithMessages.chat.userIds.contains(userId)) ZIO.unit
              else ZIO.fail(new Exception("Forbidden"))
              json <- ZIO.attempt(chatWithMessages.toJson)
            } yield Response.json(json))
              .catchAll {
                case _: Exception => ZIO.succeed(Response.text("Forbidden").status(Status.Forbidden))
                case _ => ZIO.succeed(Response.status(Status.NotFound))
              }
          case None =>
            ZIO.succeed(Response.text("Unauthorized").status(Status.Unauthorized))
        }
      }
  )
}