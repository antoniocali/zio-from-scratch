package com.antoniocali

import ourzio.*

object Main extends scala.App :
  lazy val program = for {
    _ <- console.putStrLn("Name")
    name <- ZIO.succeed("Antonio")
    _ <- ZIO.effect(throw RuntimeException("s")).catchAll(_ => console.putStrLn("Hello again"))
    _ <- console.putStrLn(s"hello $name")
  } yield ()

  Runtime.default.unsafeRuntimeAsync(program)

