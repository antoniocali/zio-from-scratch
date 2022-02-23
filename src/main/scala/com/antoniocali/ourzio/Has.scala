package com.antoniocali.ourzio

import scala.reflect.ClassTag

final class Has[A] private(private val map: Map[String, Any])
object Has:
  def apply[A](a: A)(using tag: ClassTag[A]): Has[A] =
    new Has(Map(tag.toString -> a))

  extension[A <: Has[?]] (a: A)
    infix def union[B <: Has[?]](b: B): A & B =
      new Has(a.map ++ b.map).asInstanceOf[A & B]

    inline def ++[B <: Has[?]](b: B): A & B = union(b = b)

    def get[S](using view: A => Has[S], tag: ClassTag[S]): S =
      a.map(tag.toString).asInstanceOf[S]

