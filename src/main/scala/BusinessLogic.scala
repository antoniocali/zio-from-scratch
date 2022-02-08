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

object MainDep extends scala.App :

  val program = for {
    cats <- ZIO.access[businessLogic.BusinessLogic](bs => bs.picOfTopic("cats"))
    _ <- console.putStrLn(cats.toString)
    dogs <- ZIO.access[businessLogic.BusinessLogic](_.picOfTopic("dogs"))
    _ <- console.putStrLn(dogs.toString)
  } yield ()

  val program2 = for {
    cats <- businessLogic.picOfTopic("cats")
    _ <- console.putStrLn(cats.toString)
    dogs <- businessLogic.picOfTopic("dogs")
    _ <- console.putStrLn(dogs.toString)
  } yield ()

  Runtime.default.unsafeRuntimeAsync(program2.provide(DependencyGraph.make))