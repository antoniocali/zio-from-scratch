package com.antoniocali.ourzio

object console:
  type Console = Has[Console.Service]

  def putStrLn(line: => String): ZIO[Console, Nothing, Unit] =
    ZIO.accessM[Console](_.get.putStrLn(line))

  def getStrLn: ZIO[Console, Nothing, String] =
    ZIO.accessM[Console](_.get.getStrLn)

  object Console:
    trait Service:
      def putStrLn(line: => String): ZIO[Any, Nothing, Unit]

      def getStrLn: ZIO[Any, Nothing, String]

    lazy val live: ZLayer[Any, Nothing, Console] =
      ZLayer.succeed(make)

    lazy val make: Service =
      new Service :
        def putStrLn(line: => String): ZIO[Any, Nothing, Unit] =
          ZIO.succeed {
            println(line)
          }

        lazy val getStrLn: ZIO[Any, Nothing, String] = ZIO.succeed {
          scala.io.StdIn.readLine()
        }
