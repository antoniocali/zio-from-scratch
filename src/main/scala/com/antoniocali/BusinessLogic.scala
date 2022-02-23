package com.antoniocali

import ourzio.*

object businessLogic:
  type BusinessLogic = Has[BusinessLogic.Service]

  def picOfTopic(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.get.picOfTopic(topic))

  object BusinessLogic:
    trait Service:
      def picOfTopic(topic: String): ZIO[Any, Nothing, Boolean]

    lazy val live: ZLayer[google.Google, Nothing, BusinessLogic] = ZLayer.fromService(make)

    def make(g: google.Google.Service): Service =
      new Service {
        override def picOfTopic(topic: String): ZIO[Any, Nothing, Boolean] = g.picsOf(topic).map(_ % 2 == 0)
      }

object google:
  type Google = Has[Google.Service]

  object Google:
    trait Service:
      def picsOf(topic: String): ZIO[Any, Nothing, Int]

  def picsOf(topic: String): ZIO[Google, Nothing, Int] =
    ZIO.accessM(_.get.picsOf(topic))

object GoogleImpl:
  lazy val live: ZLayer[Any, Nothing, google.Google] = ZLayer.succeed(make)

  def make: google.Google.Service =
    new :
      override def picsOf(topic: String): ZIO[Any, Nothing, Int] = ZIO.succeed(if (topic == "cats") 1 else 0)

object controller:
  type Controller = Has[Controller.Service]

  object Controller:
    trait Service:
      def run: ZIO[Any, Nothing, Unit]

    lazy val live: ZLayer[businessLogic.BusinessLogic & console.Console, Nothing, Controller] =
      ZLayer.fromServices(make)

    def make(bl: businessLogic.BusinessLogic.Service, con: console.Console.Service): Service =
      new Service {
        override def run: ZIO[Any, Nothing, Unit] = for {
          cats <- bl.picOfTopic("cats")
          _ <- con.putStrLn(cats.toString)
          dogs <- bl.picOfTopic("dogs")
          _ <- con.putStrLn(dogs.toString)
        } yield ()
      }

  lazy val run: ZIO[Controller, Nothing, Unit] =
    ZIO.accessM[Controller](_.get.run)

object DependencyGraph:
  lazy val live: ZLayer[Any, Nothing, controller.Controller] =
    for
      (google, con) <- GoogleImpl.live.zip(console.Console.live)
      bs <- businessLogic.BusinessLogic.live.provide(google)
      c <- controller.Controller.live.provide(bs ++ con)
    yield c


  lazy val make: controller.Controller.Service =
    val (google, con) = (GoogleImpl.make, console.Console.make)
    val bl = businessLogic.BusinessLogic.make(google)
    val c = controller.Controller.make(bl, con)
    c

object MainDep extends scala.App :

  lazy val program =
  //    DependencyGraph.live.flatMap(_.get.run)
  //    DependencyGraph.live.flatMap(r => controller.run.provide(Has(r)))
    controller.run.provideLayer(DependencyGraph.live)
  Runtime.default.unsafeRuntimeAsync(program)