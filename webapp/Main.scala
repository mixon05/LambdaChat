package webapp

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

@main
def LiveApp(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.view()
  )

object Main:

  def view(): Element =
    val currentPage = Var("home")
    val maybeUsername = Var(Option(dom.window.localStorage.getItem("username")))

    div(
      navBar(currentPage, maybeUsername.signal),
      child <-- currentPage.signal.map {
        case "home"     => homePage()
        case "auth"     => Login()
        case "profile"  => Profile()
        case other      => div(s"404: Unknown page '$other'")
      }
    )

  def navBar(currentPage: Var[String], maybeUser: Signal[Option[String]]): Element =
    htmlTag("nav")(
      cls := "navbar",
      button("🏠 Home", onClick.mapTo("home") --> currentPage),
      child <-- maybeUser.map {
        case Some(name) =>
          button(s"👤 $name", onClick.mapTo("profile") --> currentPage)
        case None =>
          button("🔐 Login/Register", onClick.mapTo("auth") --> currentPage)
      }
    )

  def homePage(): Element =
    div(
      h1("Welcome to LambdaChat!"),
      p("This is the home page. Use the navbar to log in or register.")
    )
