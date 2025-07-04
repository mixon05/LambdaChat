package webapp

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

@main
def LiveApp(): Unit =
  dom.window.onpopstate = _ => Main.updatePageFromLocation()
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.view()
  )


object Main:

  def navigateTo(path: String): Unit =
    dom.window.history.pushState(null, "", path)
    updatePageFromLocation()

  def extractInitialPage(): String =
    dom.window.location.pathname match
      case "/"                    => "home"
      case "/auth"                => "auth"
      case "/profile"             => "profile"
      case "/users"               => "users"
      case "/chats"               => "chats"
      case path if path.startsWith("/chat/") => "chat"
      case _                      => "404"

  def updatePageFromLocation(): Unit =
    currentPage.set(extractInitialPage())
    currentChatId.set(extractChatIdFromPath())

  def extractChatIdFromPath(): Option[String] =
    dom.window.location.pathname.stripPrefix("/").split("/").toList match
      case "chat" :: chatId :: Nil => Some(chatId)
      case _ => None

  val currentPage    = Var(extractInitialPage())
  val currentChatId  = Var(extractChatIdFromPath())
  val maybeUsername  = Var(Option(dom.window.localStorage.getItem("username")))

  def view(): Element =
    div(
      navBar(currentPage, maybeUsername.signal),
      child <-- currentPage.signal.map {
        case "home"     => homePage()
        case "auth"     => Login()
        case "profile"  => Profile()
        case "chats"    => ChatList()
        case "users"    => UserSearch()
        case "chat" =>
          div(
            child <-- currentChatId.signal.map {
              case Some(chatId) => ChatPage(chatId)
              case None         => div("No chat ID provided in URL")
            }
          )
        case other      => div(s"404: Unknown page '$other'")
      }
    )

  def navBar(currentPage: Var[String], maybeUser: Signal[Option[String]]): Element =
    htmlTag("nav")(
      cls := "navbar",
      button("Home", onClick --> (_ => navigateTo("/"))),
      button("Users", onClick --> (_ => navigateTo("/users"))),
      button("Chats", onClick --> (_ => navigateTo("/chats"))),
      child <-- maybeUser.map {
        case Some(name) =>
          button(name, onClick --> (_ => navigateTo("/profile")))
        case None =>
          button("Login/Register", onClick --> (_ => navigateTo("/auth")))
      }
    )

  def homePage(): Element =
    div(
      h1("Welcome to LambdaChat!"),
      p("This is the home page. Use the navbar to log in or register.")
    )