package scalazg.learning.day2

import scalaz._
import Scalaz._
import scalazg.S

/**
 * http://eed3si9n.com/learning-scalaz/
 */
object Functors extends App {

  {
    // Functor also enables some operators that overrides the values in the data structure like
    // >|, as, fpair, strengthL, strengthR, and void:
    S $ (List(1, 2, 3) >| "x")
    S $ (List(1, 2, 3) as "x")
    S $ List(1, 2, 3).fpair
    S $ List(1, 2, 3).strengthL("x")
    S $ List(1, 2, 3).strengthR("x")
    S $ List(1, 2, 3).void

    // [We can think of fmap as] a function that takes a function and returns a new function
    // that’s just like the old one, only it takes a functor as a parameter and returns a
    // functor as the result. It takes an a -> b function and returns a function f a -> f b.
    // This is called lifting a function.
    def f = Functor[List].lift {(_: Int) * 2}
    S $ f(List(3))


    // In Haskell, the fmap seems to be working as the same order as f compose g.
    // ghci> fmap (*3) (+100) 1
    // 303
    // ghci> (*3) . (+100) $ 1
    // 303
    // Let’s compare the declaration of fmap and Scalaz’s map operator:
    // fmap :: (a -> b) -> f a -> f b
    // and here’s Scalaz:
    // def map[B](f: A => B): F[B] = F.map(self)(f)
    // So the order is completely different. Since map here’s an injected method of F[A],
    // the data structure to be mapped over comes first, then the function comes next. Let’s see List:
    // ghci> fmap (*3) [1, 2, 3]
    // [3,6,9]
    S $ (List(1, 2, 3) map {3*})

    // Scalaz also defines Functor instance for Function1.
    // Basically map gives us a way to compose functions, except the order is in reverse from f compose g.
    def function1Map = ((x: Int) => x + 1) map {_ * 7}
    S $ function1Map(3)

    // Scalaz defines Functor instances for Tuples.
    S $ ((1, 2, 3) map {_ + 1})
  }

}
