import zio._
import mongo.MongoModule
import com.mongodb.client.MongoClient
import com.typesafe.config.ConfigFactory


object Main extends ZIOAppDefault {


  def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {

    val program = for {
      client <- MongoModule.Service.client
      _ <- ZIO.attempt {
        val dbNames = client.listDatabaseNames().iterator()
        println("Databases:")
        while (dbNames.hasNext) {
          println(s" - ${dbNames.next()}")
        }
      }
    } yield ()

    program.provideLayer(MongoModule.live)
  }
}