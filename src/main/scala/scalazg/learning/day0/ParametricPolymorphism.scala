package scalazg.learning.day0

import scalazg.S

/**
 * Exercising code in: http://eed3si9n.com/learning-scalaz/
 */
object ParametricPolymorphism extends App {

  {
    // Method injection (enrich my library)
    // If we were to write a function that sums two types using the Monoid, we need to call it like this.

    // We would like to provide an operator. But we don’t want to enrich just one type,
    // but enrich all types that has an instance for Monoid. Let me do this in Scalaz 7 style.

    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }
    object Monoid {
      implicit val IntMonoid = new Monoid[Int] {
        def mappend(a: Int, b: Int): Int = a + b
        def mzero: Int = 0
      }
      implicit val StringMonoid = new Monoid[String] {
        def mappend(a1: String, a2: String): String = a1 + a2
        def mzero: String = ""
      }
    }

    trait MonoidOp[A] {
      val F: Monoid[A]
      val value: A
      def |+|(a2: A) = F.mappend(value, a2)
    }
    implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
      val F = implicitly[Monoid[A]]
      val value = a
    }

    S $ 3 |+| 4
    S $ "a" |+| "b"
  }

  {
    // What we wanted was a function that generalized on List. …
    // So we want to generalize on foldLeft operation.
    // Now we can apply the same abstraction to pull out FoldLeft typeclass.

    trait FoldLeft[F[_]] {
      def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
    }
    object FoldLeft {
      implicit val FoldLeftList = new FoldLeft[List] {
        def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B): B = xs.foldLeft(b)(f)
      }
    }

    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }
    object Monoid {
      implicit val IntMonoid = new Monoid[Int] {
        def mappend(a: Int, b: Int): Int = a + b
        def mzero: Int = 0
      }
      implicit val StringMonoid = new Monoid[String] {
        def mappend(a1: String, a2: String): String = a1 + a2
        def mzero: String = ""
      }
    }

    def sum[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
      val m = implicitly[Monoid[A]]
      val fl = implicitly[FoldLeft[M]]
      fl.foldLeft(xs, m.mzero, m.mappend)
    }

    S $ sum(List(1, 2, 3, 4))
    S $ sum(List("a", "b", "c"))
  }

  {
    // Our sum function is pretty general now, appending any monoid in a list.
    // We can test that by writing another Monoid for String.
    // I’m also going to package these up in an object called Monoid.
    // The reason for that is Scala’s implicit resolution rules: When it needs an implicit parameter of some type,
    // it’ll look for anything in scope. It’ll include the companion object of the type that you’re looking for.

    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }
    object Monoid {
      implicit val IntMonoid = new Monoid[Int] {
        def mappend(a: Int, b: Int): Int = a + b
        def mzero: Int = 0
      }
      implicit val StringMonoid = new Monoid[String] {
        def mappend(a1: String, a2: String): String = a1 + a2
        def mzero: String = ""
      }
    }

    def sum[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero)(m.mappend)
    }

    val multiMonoid = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a * b
      def mzero: Int = 1
    }

    S $ sum(List("a", "b", "c"))
    S $ sum(List(1, 2, 3))
    // You can still provide different monoid directly to the function.
    // We could provide an instance of monoid for Int using multiplications.
    S $ sum(List(1, 2, 3, 4))(multiMonoid)
  }

  {
    // If we try to generalize a little bit. I’m going to pull out a thing called Monoid. …
    // It’s a type for which there exists a function mappend, which produces another type in the same set;
    // and also a function that produces a zero.

    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }

    object IntMonoid extends Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    // Option 1
    // Make the Monoid implicit so we don’t have to specify it each time.
    def sum1[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)

    // Option 2
    // The implicit parameter is often written as a context bound
    def sum2[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero)(m.mappend)
    }

    implicit val intMonoid = IntMonoid

    S $ sum1(List(1, 2, 3, 4))
    S $ sum2(List(1, 2, 3, 4))

  }

  {
    // Ad-hoc polymorphism (as opposed to Subtype polymorphism)

    trait Plus[A] {
      def plus(a1: A, a2: A): A
    }

    object IntPlus extends Plus[Int] {
      def plus(a1: Int, a2: Int): Int = a1 + a2
    }

    implicit val intPlus = IntPlus

    def plus[A: Plus](a1: A, a2: A): A = implicitly[Plus[A]].plus(a1, a2)

    S $ plus(1, 2)
  }

  {
    // In this function head, it takes a list of A’s, and returns an A.
    // And it doesn’t matter what the A is: It could be Ints, Strings, Oranges, Cars, whatever.
    // Any A would work, and the function is defined for every A that there can be.

    def head[A](xs: List[A]): A = xs(0)
    S $ head(1 :: 2 :: Nil)

    case class Car(make: String)
    S $ head(Car("Civic") :: Car("CR-V") :: Nil)
  }


}
