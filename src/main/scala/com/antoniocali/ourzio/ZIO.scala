package com.antoniocali
package ourzio

import scala.reflect.ClassTag

final class ZIO[-R, +E, +A](val run: R => Either[E, A]):

  def flatMap[R1 <: R, E1 >: E, B](azb: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] = ZIO { r =>
    val errorOrA = run(r)
    val zErrorOrB = errorOrA match
      case Left(e) => ZIO.fail(e)
      case Right(a) => azb(a)
    val errorOrB = zErrorOrB.run(r)
    errorOrB
  }

  def zip[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
    val result: ZIO[R1, E1, (A, B)] = for {
      a <- this
      b <- that
    } yield a -> b
    result


  def map[B](ab: A => B): ZIO[R, E, B] = ZIO {
    r =>
      val errorOrA = run(r)
      errorOrA match
        case Left(e) => Left(e)
        case Right(a) => Right(ab(a))
  }

  def catchAll[R1 <: R, F >: E, A1 >: A](h: E => ZIO[R1, F, A1]): ZIO[R1, F, A1] = ZIO {
    (r) =>
      val errorOrA = run(r)
      val zErrorFOrA = errorOrA match
        case Left(e) => h(e)
        case Right(a) => ZIO.succeed(a)
      val errorFOrA = zErrorFOrA.run(r)
      errorFOrA
  }

  def mapError[F](h: E => F): ZIO[R, F, A] = ZIO {
    (r) =>
      val errorForA = run(r)
      errorForA match
        case Left(e) => Left(h(e))
        case Right(a) => Right(a)
  }

  def provide(r: => R): ZIO[Any, E, A] =
    ZIO(_ => run(r))

  def provideLayer[R1, E1 >: E, B](layer: ZLayer[R1, E1, B])(using view: B => R): ZIO[R1, E1, A] =
    layer.zio.map(view).flatMap(r => provide(r))


  def provideCustomLayer[E1 >: E, B <: Has[?]](layer: ZLayer[ZEnv, E1, B])(using view: B => R): ZIO[ZEnv, E1, A] =
    provideSomeLayer(layer)

  def provideSome[R0](f: R0 => R): ZIO[R0, E, A] =
    for {
      r0 <- ZIO.environment[R0]
      r = f(r0)
      a <- provide(r)
    } yield a

  def provideSomeLayer[R0 <: Has[?], E1 >: E, B <: Has[?]](layer: ZLayer[R0, E1, B])(using view: B => R): ZIO[R0, E1, A] =
    provideLayer(ZLayer.identity[R0] ++ layer)

object ZIO:
  def succeed[A](a: => A): ZIO[Any, Nothing, A] = ZIO {
    _ => Right(a)
  }

  def fail[E](e: => E): ZIO[Any, E, Nothing] = ZIO {
    _ => Left(e)
  }

  def effect[A](a: => A): ZIO[Any, Throwable, A] = ZIO { r =>
    try Right(a)
    catch Left(_)
  }

  def fromFunction[R, A](run: R => A): ZIO[R, Nothing, A] = ZIO {
    r => Right(run(r))
  }

  inline def environment[R]: ZIO[R, Nothing, R] = ZIO.identity

  def identity[R]: ZIO[R, Nothing, R] = ZIO.fromFunction(Predef.identity)

  def access[R]: AccessPartiallyApplied[R] = AccessPartiallyApplied()

  def accessM[R]: AccessMPartiallyApplied[R] = AccessMPartiallyApplied()

