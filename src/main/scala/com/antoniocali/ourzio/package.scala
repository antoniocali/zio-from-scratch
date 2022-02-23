package com.antoniocali

package object ourzio:
  final class AccessPartiallyApplied[R]():
    def apply[A](r: R => A): ZIO[R, Nothing, A] =
      ZIO.environment[R].map(r)

  final case class AccessMPartiallyApplied[R]():
    def apply[E, A](r: R => ZIO[R, E, A]): ZIO[R, E, A] =
      ZIO.environment[R].flatMap(r)

  type ZEnv = Has[console.Console.Service]

