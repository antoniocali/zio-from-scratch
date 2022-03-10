package com.antoniocali

import ourzio.*

import java.io.IOException

object businessLogic:
  type BusinessLogic = Has[BusinessLogic.Service]

  def picOfTopic(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.get.picOfTopic(topic))

  object BusinessLogic:
    trait Service:
      def picOfTopic(topic: String): ZIO[Any, Nothing, Boolean]

    lazy val live: ZLayer[google.Google, Nothing, BusinessLogic] = ZLayer.fromService(make)

    lazy val any: ZLayer[BusinessLogic, Nothing, BusinessLogic] = ZLayer.requires

    def make(g: google.Google.Service): Service =
      new Service {
        override def picOfTopic(topic: String): ZIO[Any, Nothing, Boolean] = g.picsOf(topic).map(_ % 2 == 0)
      }

object google:
  type Google = Has[Google.Service]

  object Google:
    trait Service:
      def picsOf(topic: String): ZIO[Any, Nothing, Int]

    lazy val any: ZLayer[Google, Nothing, Google] = ZLayer.requires


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
      def run: ZIO[Any, IOException, Unit]

    lazy val live: ZLayer[businessLogic.BusinessLogic & console.Console, Nothing, Controller] =
      ZLayer.fromServices(make)

    lazy val any: ZLayer[Controller, Nothing, Controller] = ZLayer.requires

    def make(bl: businessLogic.BusinessLogic.Service, con: console.Console.Service): Service =
      new Service {
        override def run: ZIO[Any, IOException, Unit] = for {
          cats <- bl.picOfTopic("cats")
          _ <- con.putStrLn(cats.toString)
          dogs <- bl.picOfTopic("dogs")
          _ <- con.putStrLn(dogs.toString)
        } yield ()
      }

  lazy val run: ZIO[Controller, IOException, Unit] =
    ZIO.accessM[Controller](_.get.run)

object DependencyGraph:

  lazy val env: ZLayer[Any, Nothing, controller.Controller] =
    GoogleImpl.live >>> businessLogic.BusinessLogic.live ++
      console.Console.live >>> controller.Controller.live

  lazy val partial: ZLayer[console.Console, Nothing, controller.Controller] =
    (GoogleImpl.live >>> businessLogic.BusinessLogic.live) ++
      console.Console.any >>> controller.Controller.live

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

  object FancyConsole:
    lazy val any: ZLayer[console.Console, Nothing, console.Console] = ZLayer.requires
    lazy val make: console.Console.Service = new console.Console.Service {
      override def putStrLn(line: => String): ZIO[Any, IOException, Unit] =
        ZIO.succeed(println(scala.Console.GREEN + line + scala.Console.RESET))


      override def getStrLn: ZIO[Any, IOException, String] =
        ZIO.succeed(scala.io.StdIn.readLine)
    }

    lazy val live: ZLayer[Any, Nothing, console.Console] = ZLayer.succeed(make)


  lazy val program =
  //    DependencyGraph.live.flatMap(_.get.run)
  //    DependencyGraph.live.flatMap(r => controller.run.provide(Has(r)))
  //    controller.run.provideLayer(DependencyGraph.env)
    controller.run.provideLayer(FancyConsole.live >>> DependencyGraph.partial)
  Runtime.default.unsafeRuntimeAsync(program)