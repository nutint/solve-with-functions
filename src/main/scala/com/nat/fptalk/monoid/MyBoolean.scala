package com.nat.fptalk.monoid

import cats.Monoid

trait MyBoolean
case object MyFalse extends MyBoolean
case object MyTrue extends MyBoolean

/**
  * For better understanding please review the following link for monoid implementation
  * https://typelevel.org/cats/typeclasses/monoid.html
  */
object MyBoolean {

  /**
    * Or operation of boolean implemented as Monoid
    */
  object MyBooleanAndMonoid extends Monoid[MyBoolean] {
    override def combine(x: MyBoolean, y: MyBoolean): MyBoolean = (x, y) match {
      case (MyTrue, MyTrue) => MyTrue
      case _ => MyFalse
    }

    /**
      * And implementation of boolean implemented as Monoid
      * @return
      */
    override def empty: MyBoolean = MyTrue
  }

  object MyBooleanOrMonoid extends Monoid[MyBoolean] {
    override def combine(x: MyBoolean, y: MyBoolean): MyBoolean = (x, y) match {
      case (MyTrue, _) => MyTrue
      case (_, MyTrue) => MyTrue
      case _ => MyFalse
    }

    override def empty: MyBoolean = MyFalse
  }
}
