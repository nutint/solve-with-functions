package com.nat.fptalk.monad

import cats._

case class DoNothing[A](value: A)

object DoNothing {
  implicit val dnFunctor: Functor[DoNothing] = new Functor[DoNothing] {
    override def map[A, B](fa: DoNothing[A])(f: A => B): DoNothing[B] = DoNothing(f(fa.value))
  }

  implicit def dnApp(implicit dnFunctor: Functor[DoNothing]): Applicative[DoNothing] = new Applicative[DoNothing] {
    override def pure[A](x: A): DoNothing[A] = DoNothing(x)

    override def ap[A, B](ff: DoNothing[A => B])(fa: DoNothing[A]): DoNothing[B] = DoNothing(ff.value(fa.value))
  }

  implicit def dnMonad(implicit dnApp: Applicative[DoNothing]): Monad[DoNothing] = new Monad[DoNothing] {
    override def flatMap[A, B](fa: DoNothing[A])(f: A => DoNothing[B]): DoNothing[B] = f(fa.value)

    override def flatten[A](fa: DoNothing[DoNothing[A]]): DoNothing[A] = fa.value

    override def tailRecM[A, B](a: A)(f: A => DoNothing[Either[A, B]]): DoNothing[B] =
      f(a) match {
        case DoNothing(Right(b)) => DoNothing(b)
        case DoNothing(Left(aa)) => tailRecM(aa)(f)
      }

    override def pure[A](x: A): DoNothing[A] = dnApp.pure(x)
  }
}
