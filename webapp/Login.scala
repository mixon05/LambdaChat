package webapp

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.concurrent.ExecutionContext.Implicits.global

object Login:

  def apply(): Element =
    val isLoginMode = Var(true)
    val username = Var("")
    val password = Var("")
    val message  = Var("")

    if dom.window.localStorage.getItem("username") != null then
      dom.window.location.href = "/"

    def submit(): Unit =
      val user = username.now()
      val pass = password.now()
      val json = s"""{"username":"$user","password":"$pass"}"""
      val endpoint =
        if isLoginMode.now() then "http://localhost:8080/login"
        else "http://localhost:8080/register"

      Ajax.post(
        url = endpoint,
        data = json,
        headers = Map("Content-Type" -> "application/json")
      ).map { xhr =>
        if xhr.status == 200 then
          val responseJson = JSON.parse(xhr.responseText).asInstanceOf[js.Dynamic]
          val token = responseJson.token.asInstanceOf[String]
          val userId = responseJson.userId.asInstanceOf[String]

          dom.window.localStorage.setItem("username", user)
          dom.window.localStorage.setItem("token", token)
          dom.window.localStorage.setItem("userId", userId)

          dom.window.location.href = "/"
        else if xhr.status == 401 then
          message.set("Unauthorized: Wrong username or password")
        else
          message.set(s"Server error (${xhr.status})")
      }.recover {
        case _ => message.set("Network error")
      }

    div(
      cls := "auth-container",
      h2(
        child.text <-- isLoginMode.signal.map(if _ then "Login" else "Register")
      ),
      div(cls := "auth-toggle",
        button(
          cls := "auth-button",
          onClick.mapTo(true) --> isLoginMode.writer,
          disabled <-- isLoginMode,
          "Login"
        ),
        button(
          cls := "auth-button",
          onClick.mapTo(false) --> isLoginMode.writer,
          disabled <-- isLoginMode.signal.map(!_),
          "Register"
        )
      ),
      form(
        cls := "auth-form",
        onSubmit.preventDefault --> { _ => submit() },
        input(
          cls := "auth-input",
          tpe := "text",
          placeholder := "Username",
          controlled(
            value <-- username,
            onInput.mapToValue --> username
          )
        ),
        input(
          cls := "auth-input",
          tpe := "password",
          placeholder := "Password",
          controlled(
            value <-- password,
            onInput.mapToValue --> password
          )
        ),
        button(
          cls := "auth-submit",
          tpe := "submit",
          child.text <-- isLoginMode.signal.map(if _ then "Log in" else "Register")
        )
      ),
      div(cls := "auth-message", child.text <-- message.signal)
    )