package com.antoniocali.ourzio

import scala.reflect.ClassTag

final class ZLayer[-R, +E, +A](val zio: ZIO[R, E, A]):

  inline def flatMap[R1 <: R, E1 >: E, B](azb: A => ZLayer[R1, E1, B]): ZLayer[R1, E1, B] =
    ZLayer(this.zio.flatMap(a => azb(a).zio))

  inline def map[B](f: A => B): ZLayer[R, E, B] =
    ZLayer(this.zio.map(f))

  inline def zip[R1 <: R, E1 >: E, B](that: ZLayer[R1, E1, B]): ZLayer[R1, E1, (A, B)] =
    ZLayer(this.zio.zip(that.zio))

  inline def provideSome[R0](f: R0 => R): ZLayer[R0, E, A] =
    ZLayer(this.zio.provideSome(f))

  inline def provide(r: => R): ZLayer[Any, E, A] =
    ZLayer(this.zio.provide(r))


object ZLayer:
  def succeed[A: ClassTag](a: A): ZLayer[Any, Nothing, Has[A]] =
    ZLayer(ZIO.succeed(Has(a)))

  def fromService[R <: Has[S], S: ClassTag, A: ClassTag](f: S => A): ZLayer[R, Nothing, Has[A]] =
    ZLayer(ZIO.fromFunction { r =>
      val service = r.get[S]
      val a = f(service)
      Has(a)
    })

  def fromServices[R <: Has[S1] & Has[S2], S1: ClassTag, S2: ClassTag, A: ClassTag](f: (S1, S2) => A): ZLayer[R, Nothing, Has[A]] =
    ZLayer(ZIO.fromFunction { r =>
      val s1 = r.get[S1]
      val s2 = r.get[S2]
      val a = f(s1, s2)
      Has(a)
    })