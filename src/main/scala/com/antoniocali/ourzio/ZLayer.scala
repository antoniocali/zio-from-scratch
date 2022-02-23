package com.antoniocali.ourzio

import scala.reflect.ClassTag

object ZLayer:
  def succeed[A : ClassTag](a: A): ZIO[Any, Nothing, Has[A]] =
    ZIO.succeed(Has(a))
