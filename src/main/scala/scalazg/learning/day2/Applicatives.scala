package scalazg.learning.day2

import scalaz._
import Scalaz._
import scalazg.S

/**
 * http://eed3si9n.com/learning-scalaz/
 */
object Applicatives extends App {

  {
    // LYAHFGG: Let’s try implementing a function that takes a list of applicatives and
    // returns an applicative that has a list as its result value. We’ll call it sequenceA.
    def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
      case Nil => (Nil: List[A]).point[F]
      case x :: xs => (x |@| sequenceA(xs)) {_ :: _}
    }
    S $ sequenceA(List(1.some, 2.some))
    S $ sequenceA(List(1.some, none, 2.some))
    S $ sequenceA(List(List(1, 2, 3), List(4, 5, 6)))

    // LYAHFGG: Control.Applicative defines a function that’s called liftA2
    def withLift2 = Apply[Option].lift2((_: Int) :: (_: List[Int]))
    S $ withLift2(3.some, List(4).some)

    // LYAHFGG: Lists (actually the list type constructor, []) are applicative functors. What a surprise!
    S $ (List(1, 2, 3) <*> List((_: Int) * 0, (_: Int) + 100, (x: Int) => x * x))
    S $ (List("ha", "heh", "hmm") |@| List("?", "!", ".")) {_ + _}

    // Extract values from containers and apply them to a single function
    S $ (3.some |@| 5.some) {_ + _} // leads to an allocation of wrapper object
    S $ Apply[Option].map2(3.some, 5.some) {_ + _} // alternative version without wrapper allocation

    // LYAHFGG:
    // You can think of <*> as a sort of a beefed-up fmap. Whereas fmap takes a function and a functor and
    // applies the function inside the functor value, <*> takes a functor that has a function in it and
    // another functor and extracts that function from the first functor and then maps it over the second one.
    S $ (9.some <*> {(_: Int) + 3}.some)
    S $ (9.some <* {(_: Int) + 3}.some)
    S $ (9.some *> {(_: Int) + 3}.some)

    // LYAHFGG: Meet the Applicative typeclass. It lies in the Control.Applicative module and
    // it defines two methods, pure and <*>.
    // In Scalaz: So Applicative extends another typeclass Apply, and introduces point and its alias pure
    // LYAHFGG: pure should take a value of any type and return an applicative value with that value inside it. …
    // A better way of thinking about pure would be to say that it takes a value and puts it in some sort of
    // default (or pure) context—a minimal context that still yields that value.
    // Scalaz: Scalaz likes the name point instead of pure, and it seems like it’s basically a constructor that takes
    // value A and returns F[A]. It doesn’t introduce an operator, but it intoduces point method and its symbolic
    // alias η to all data types.
    S $ 1.point[List]
    S $ 1.point[Option]
    S $ (1.point[Option] map {_ + 2})
    S $ (1.point[List] map {_ + 2})

    // So far, when we were mapping functions over functors, we usually mapped functions that take only one parameter.
    // But what happens when we map a function like *, which takes two parameters, over a functor?
    // The following throws an error: type mismatch; found   : (Int, Int) => Int required: Int => ?
    // S $ (List(1, 2, 3, 4) map {(_: Int) * (_:Int)})
    // Oops. We have to curry this:
    def withMap = (List(1, 2, 3, 4) map {(_: Int) * (_:Int)}.curried)
    S $ withMap.map({_(9)})
  }


}
