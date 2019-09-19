package com.nat.fptalk.monad

import cats._

/**
  * For better understanding this code
  * Please review https://typelevel.org/cats/typeclasses/monad.html for more details
  * @tparam A
  */
sealed trait OptionA[+A]
case class SomeA[A](a: A) extends OptionA[A]
case object NoneA extends OptionA[Nothing]

object OptionA {
  def apply[A](a: A): OptionA[A] = SomeA(a)
  def unapply[A](arg: OptionA[A]): Option[A] = arg match {
    case SomeA(v) => Some(v)
    case NoneA => None
  }
  implicit val dnFunctor: Functor[OptionA] = new Functor[OptionA] {
    override def map[A, B](fa: OptionA[A])(f: A => B): OptionA[B] = fa match {
      case SomeA(v) => SomeA(f(v))
      case NoneA => NoneA
    }
  }
  implicit def dnApp: Applicative[OptionA] = new Applicative[OptionA] {
    override def pure[A](x: A): OptionA[A] = SomeA(x)
    override def ap[A, B](ff: OptionA[A => B])(fa: OptionA[A]): OptionA[B] = (ff, fa) match {
      case (SomeA(f), SomeA(v)) => SomeA(f(v))
      case (NoneA, SomeA(_)) | (SomeA(_), NoneA) | (NoneA, NoneA) => NoneA
    }
  }

  implicit def dnMonad(implicit dnApp: Applicative[OptionA]): Monad[OptionA] = new Monad[OptionA] {
    override def flatMap[A, B](fa: OptionA[A])(f: A => OptionA[B]): OptionA[B] = fa match {
      case SomeA(v) => f(v)
      case NoneA => NoneA
    }

    @scala.annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => OptionA[Either[A, B]]): OptionA[B] =
      f(a) match {
        case SomeA(Right(b)) => SomeA(b)
        case SomeA(Left(aa)) => tailRecM(aa)(f)
        case NoneA => NoneA
      }
    override def pure[A](x: A): OptionA[A] = dnApp.pure(x)
  }
}