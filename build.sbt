ThisBuild / version := "0.1.0-SNAPSHOT"
import org.scalajs.linker.interface.ModuleSplitStyle

ThisBuild / scalaVersion := "3.3.5"

lazy val server = (project in file("server"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"            %% "zio"                 % "2.1.17",
      "dev.zio"               %% "zio-json"            % "0.7.43",
      "dev.zio"               %% "zio-http"            % "3.2.0",
      "com.typesafe" % "config" % "1.4.3",
      "org.mongodb"           %  "mongodb-driver-sync" % "5.5.0",
    ),
  )
lazy val webapp = (project in file("webapp"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("webapp")))
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo" %%% "laminar" % "17.0.0",
    )
  )


lazy val root = (project in file("."))
  .aggregate(
    server,
    webapp
  )
  .settings(
    name := "LambdaChat"
  )
