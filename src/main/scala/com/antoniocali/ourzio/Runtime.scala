package com.antoniocali.ourzio

object Runtime:
  object default:
    def unsafeRuntimeAsync[E, A](zio: => ZIO[ZEnv, E, A]): Either[E, A] =
      zio.provideLayer(ZEnv.live).run(())
//      zio.run(Has(console.Console.make))
