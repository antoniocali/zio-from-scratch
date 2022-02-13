package com.antoniocali

import ourzio.*


object businessLogic:
  trait BusinessLogic:
    def picOfTopic(topic: String): ZIO[Any, Nothing, Boolean]

  def picOfTopic(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.picOfTopic(topic))

  object BusinessLogic:
    lazy val live: ZIO[Google, Nothing, BusinessLogic] = ZIO.fromFunction(make)

    def make(google: Google): BusinessLogic =
      new BusinessLogic {
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
  lazy val live =
    for {
      google <- GoogleImpl.live
      bs <- businessLogic.BusinessLogic.live.provide(google)
    } yield bs


  lazy val make: businessLogic.BusinessLogic =
    val google = GoogleImpl.make
    val bs = businessLogic.BusinessLogic.make(google)
    bs


trait HasConsole:
  def console: ourzio.console.Console

trait HasBusinessLogic:
  def businessLogic: com.antoniocali.businessLogic.BusinessLogic

object MainDep extends scala.App :

  val makeProgram2 = for {
    cats <- ZIO.accessM[businessLogic.BusinessLogic](bs => bs.picOfTopic("cats"))
    _ <- console.putStrLn(cats.toString)
    dogs <- ZIO.accessM[businessLogic.BusinessLogic](_.picOfTopic("dogs"))
    _ <- console.putStrLn(dogs.toString)
  } yield ()

  lazy val program =
    for {
      bl <- DependencyGraph.live
      p <- makeProgram.provide(Has(bl) ++ Has(console.Console.make))
    } yield p

  val makeProgram = for {
    env <- ZIO.environment[Has[console.Console] & Has[businessLogic.BusinessLogic]]
    cs = env.get[console.Console]
    bl = env.get[businessLogic.BusinessLogic]
    df = env.get[Google]
    cats <- bl.picOfTopic("cats")
    _ <- cs.putStrLn(cats.toString)
    dogs <- bl.picOfTopic("dogs")
    _ <- cs.putStrLn(dogs.toString)
  } yield ()

  Runtime.default.unsafeRuntimeAsync(program)