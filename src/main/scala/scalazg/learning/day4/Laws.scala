package scalazg.learning.day4

import scalaz._
import Scalaz._
import scalazg._

/**
 * http://eed3si9n.com/learning-scalaz/
 */
object Laws extends App {

  {
    // LYAHFGG: If we want a monoid on Maybe a such that the second parameter is kept if both parameters
    // of mappend are Just values, Data.Monoid provides a the Last a type.
    S $ (Tags.Last('a'.some) |+| Tags.Last('b'.some))
    S $ (Tags.Last(none: Option[Char]) |+| Tags.Last('b'.some))

    // LYAHFGG: But if we don’t know if the contents are monoids, we can’t use mappend between them,
    // so what are we to do? Well, one thing we can do is to just discard the second value and
    // keep the first one. For this, the First a type exists.
    // Scalaz: use Tagged types
    S $ (Tags.First('a'.some) |+| Tags.First('b'.some))
    S $ (Tags.First(none: Option[Char]) |+| Tags.First('b'.some))

    // LYAHFGG: One way is to treat Maybe a as a monoid only if its type parameter a is a monoid as well
    // and then implement mappend in such a way that it uses the mappend operation of the values
    // that are wrapped with Just.
    S $ ((none: Option[String]) |+| "andy".some)
    S $ ((Ordering.LT: Ordering).some |+| none)
  }

  {
    // Monoid laws: semigroup plus:
    S $ (Monoid[Int].zero |+| 2)
    S $ (2 |+| Monoid[Int].zero)

    // Semigroup laws: associative laws covered in Monoids example in day 3.

    // The second law says that composing two functions and then mapping the resulting function over a functor
    // should be the same as first mapping one function over the functor and then mapping the other one.

    // LYAHFGG: All functors are expected to exhibit certain kinds of functor-like properties and behaviors. …
    // The first functor law states that if we map the id function over a functor, the functor that we get back
    // should be the same as the original functor.
    S $ (List(1, 2, 3) map identity assert_=== List(1, 2, 3))

  }

}
