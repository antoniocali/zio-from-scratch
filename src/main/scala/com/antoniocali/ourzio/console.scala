package com.antoniocali.ourzio

import java.io.IOException

object console:
  type Console = Has[Console.Service]

  def putStrLn(line: => String): ZIO[Console, IOException, Unit] =
    ZIO.accessM[Console](_.get.putStrLn(line))

  def getStrLn: ZIO[Console, IOException, String] =
    ZIO.accessM[Console](_.get.getStrLn)

  object Console:
    trait Service:
      def putStrLn(line: => String): ZIO[Any, IOException, Unit]

      def getStrLn: ZIO[Any, IOException, String]

    lazy val any: ZLayer[Console, Nothing, Console] =
      ZLayer.requires

    lazy val live: ZLayer[Any, Nothing, Console] =
      ZLayer.succeed(make)

    lazy val make: Service =
      new Service :
        def putStrLn(line: => String): ZIO[Any, IOException, Unit] =
          ZIO.succeed {
            println(line)
          }

        lazy val getStrLn: ZIO[Any, IOException, String] = ZIO.succeed {
          scala.io.StdIn.readLine()
        }
