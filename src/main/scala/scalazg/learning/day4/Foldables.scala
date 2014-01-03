package scalazg.learning.day4

import scalaz._
import Scalaz._
import scalazg._

/**
 * http://eed3si9n.com/learning-scalaz/
 */
object Foldables extends App {

  {
    // Let’s try the foldMap operator. Monoid[A] gives us zero and |+|, so that’s enough information to fold things over.
    // Since we can’t assume that Foldable contains a monoid we need a function to change from A => B where [B: Monoid]
    S $ (List(1, 2, 3) foldMap identity)                          // Int = 6 (implicit monoid for Int)
    S $ (List(true, false, true, true) foldMap Tags.Disjunction)  // scalaz.@@[Boolean,scalaz.Tags.Disjunction] = true

    // LYAHFGG: Because there are so many data structures that work nicely with folds,
    // the Foldable type class was introduced. Much like Functor is for things that
    // can be mapped over, Foldable is for things that can be folded up!
    S $ List(1, 2, 3).foldRight (1) {_ * _} // already present in collection libraries
    S $ 9.some.foldLeft (2) {_ + _}         // already present in collection libraries
  }

}
