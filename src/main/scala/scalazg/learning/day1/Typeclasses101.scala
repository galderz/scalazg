package scalazg.learning.day1

import scalaz._
import Scalaz._
import scalazg.S

/**
 * Exercising code in: http://eed3si9n.com/learning-scalaz/
 */
object Typeclasses101 extends App {

  {
    trait CanTruthy[A] { self =>
      /** @return true, if `a` is truthy. */
      def truthys(a: A): Boolean
    }
    object CanTruthy {
      def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev
      def truthys[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
        def truthys(a: A): Boolean = f(a)
      }
    }
    trait CanTruthyOps[A] {
      def self: A
      implicit def F: CanTruthy[A]
      final def truthy: Boolean = F.truthys(self)
    }
    object ToCanIsTruthyOps {
      implicit def toCanIsTruthyOps[A](v: A)(implicit ev: CanTruthy[A]) =
        new CanTruthyOps[A] {
          def self: A = v
          implicit def F: CanTruthy[A] = ev
        }
    }

    import ToCanIsTruthyOps._

    implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.truthys({
        case 0 => false
        case _ => true
      })

    implicit def listCanTruthy[A]: CanTruthy[List[A]] = CanTruthy.truthys({
      case Nil => false
      case _ => true
    })

    implicit val nilCanTruthy: CanTruthy[scala.collection.immutable.Nil.type] = CanTruthy.truthys({
      case _ => false
    })

    implicit val booleanCanTruthy: CanTruthy[Boolean] = CanTruthy.truthys({identity})

    S $ 10.truthy
    S $ 0.truthy
    S $ List("foo").truthy
    S $ Nil.truthy
    S $ true.truthy
    S $ false.truthy

    // Now let’s make a function that mimics the if statement, but that works with YesNo values.

    def truthyIf[A: CanTruthy, B, C](cond: A)(ifyes: => B)(ifno: => C) =
      if (cond.truthy) ifyes
      else ifno

    S $ truthyIf (Nil) {"YEAH!"} {"NO!"}
    S $ truthyIf (2 :: 3 :: 4 :: Nil) {"YEAH!"} {"NO!"}
    S $ truthyIf (true) {"YEAH!"} {"NO!"}
  }

  { // Bounded
    // Bounded members have an upper and a lower bound.
    // Scalaz equivalent for Bounded seems to be Enum as well.
  }

  { // Enum
    // Enum members are sequentially ordered types — they can be enumerated.
    // The main advantage of the Enum typeclass is that we can use its types in list ranges.
    // They also have defined successors and predecesors, which you can get with the succ and pred functions.
    // Scalaz equivalent for the Enum typeclass is Enum:
    S $ ('a' to 'e')
    S $ ('a' |-> 'e')
    S $ (3 |=> 5)
    S $ 'B'.succ
  }

  { // Read
    // Read is sort of the opposite typeclass of Show.
    // The read function takes a string and returns a type which is a member of Read.
    // I could not find Scalaz equivalent for this typeclass.
  }

  { // Show
    // Members of Show can be presented as strings.
    // Scalaz equivalent for the Show typeclass is Show:
    S $ 3.show
    S $ 3.shows
    "hello".println // prints "hello", as opposed to hello
  }

  { // Order
    // Ord is for types that have an ordering.
    // Ord covers all the standard comparing functions such as >, <, >= and <=.
    // Scalaz equivalent for the Ord typeclass is Order:

    // gt better than >
    S $ 1 > 2.0
    // S $ (1 gt 2.0)        // error: could not find implicit value for parameter F0: scalaz.Order[Any]
  }

  { // Equal
    // Eq is used for types that support equality testing.
    // The functions its members implement are == and /=.
    // Scalaz equivalent for the Eq typeclass is called Equal:
    S $ 1 === 1
    S $ 1 === 2

    // === is better than ==
    // === would fail compilation if you tried to compare Int and String.
    S $ 1 == "foo"        // warning: comparing values of types Int and String using `==' will always yield false
    // S $ 1 === "foo"    // error: could not find implicit value for parameter F0: scalaz.Equal[Object]

    // =/= is better than !=
    S $ 1 =/= 2
    S $ 1 != 2
    S $ 1 != "String"     // warning: comparing values of types Int and String using `!=' will always yield true
    // S $ 1 =/= "String" // error: could not find implicit value for parameter F0: scalaz.Equal[Object]

    try {
      1 assert_=== 2
    } catch {
      case r: RuntimeException => r.printStackTrace() // expected
    }


  }

}
