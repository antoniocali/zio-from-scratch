package com.antoniocali

package object ourzio:
  final class AccessPartiallyApplied[R]():
    def apply[A](r: R => A): ZIO[R, Nothing, A] =
      ZIO.environment[R].map(r)

  final case class AccessMPartiallyApplied[R]():
    def apply[E, A](r: R => ZIO[R, E, A]): ZIO[R, E, A] =
      ZIO.environment[R].flatMap(r)

  type ZEnv = console.Console

  object ZEnv:
    lazy val live: ZLayer[Any, Nothing, ZEnv] =
      console.Console.live

    lazy val any: ZLayer[ZEnv, Nothing, ZEnv] =
      ZLayer.requires

