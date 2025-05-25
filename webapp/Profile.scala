package webapp

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import scala.concurrent.ExecutionContext.Implicits.global

object Profile:
  def apply():  Element =
    div(
      cls := "auth-container",
      div(cls := "auth-toggle",
        button(
          cls := "auth-button",
          "Log out",
          onClick --> { _ =>
            dom.window.localStorage.removeItem("username")
            dom.window.location.reload()
          }
        ),
      ),
    )