package com.antoniocali

import com.antoniocali.businessLogic.BusinessLogic
import com.antoniocali.ourzio.console.Console
import ourzio.*
object businessLogic:
  type BusinessLogic = Has[BusinessLogic.Service]

  def picOfTopic(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.get.picOfTopic(topic))

  object BusinessLogic:
    trait Service:
      def picOfTopic(topic: String): ZIO[Any, Nothing, Boolean]

    lazy val live: ZIO[Google, Nothing, Service] = ZIO.fromFunction(make)

    def make(google: Google): Service =
      new Service {
        override def picOfTopic(topic: String): ZIO[Any, Nothing, Boolean] = google.picsOf(topic).map(_ % 2 == 0)
      }

trait Google:
  def picsOf(topic: String): ZIO[Any, Nothing, Int]

object GoogleImpl:
  lazy val live: ZIO[Any, Nothing, Google] = ZIO.succeed(make)

  def make: Google = new Google {
    override def picsOf(topic: String): ZIO[Any, Nothing, Int] = ZIO.succeed(if (topic == "cats") 1 else 0)
  }

object DependencyGraph:
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic.Service] =
    for {
      google <- GoogleImpl.live
      bs <- businessLogic.BusinessLogic.live.provide(google)
    } yield bs


  lazy val make: businessLogic.BusinessLogic.Service =
    val google = GoogleImpl.make
    val bs = businessLogic.BusinessLogic.make(google)
    bs

object MainDep extends scala.App :

  lazy val program =
    for {
      bl <- DependencyGraph.live
      p <- makeProgram.provideCustom(bl)
    } yield p

  val makeProgram: ZIO[Console & BusinessLogic, Nothing, Unit] = for {
    cats <- businessLogic.picOfTopic("cats")
    _ <- console.putStrLn(cats.toString)
    dogs <- businessLogic.picOfTopic("dogs")
    _ <- console.putStrLn(dogs.toString)
  } yield ()

  Runtime.default.unsafeRuntimeAsync(program)