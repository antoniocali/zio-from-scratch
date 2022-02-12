package com.antoniocali

import ourzio.*

trait BusinessLogic:
  def picOfTopic(topic: String): Boolean

object BusinessLogic:

  lazy val live = ZIO.fromFunction(make)

  def make(google: Google): BusinessLogic =
    new BusinessLogic {
      override def picOfTopic(topic: String): Boolean = google.picsOf(topic) % 2 == 0
    }

trait Google:
  def picsOf(topic: String): Int

object GoogleImpl:
  lazy val live: ZIO[Any, Nothing, Google] = ZIO.succeed(make)

  def make: Google = new Google {
    override def picsOf(topic: String): Int = if (topic == "cats") 1 else 0
  }

object DependencyGraph:
  lazy val live =
    for {
      google <- GoogleImpl.live
      bs <- BusinessLogic.live.provide(google)
    } yield bs


  lazy val make: BusinessLogic =
    val google = GoogleImpl.make
    val businessLogic = BusinessLogic.make(google)
    businessLogic

object MainDep extends scala.App :

  val program = for {
    dg <- DependencyGraph.live
    _ <- console.putStrLn(dg.picOfTopic("cats").toString)
    _ <- console.putStrLn(dg.picOfTopic("dogs").toString)
  } yield ()

  Runtime.default.unsafeRuntimeAsync(program)