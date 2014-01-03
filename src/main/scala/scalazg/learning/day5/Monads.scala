package scalazg.learning.day5

import scalaz._
import Scalaz._
import scalazg._

/**
 * http://eed3si9n.com/learning-scalaz/
 */
object Monads extends App {

  { // http://eed3si9n.com/learning-scalaz/Monad+laws.html

    // LYAHFGG: The final monad law (associativity) says that when we have a chain of
    // monadic function applications with >>=, it shouldn’t matter how they’re nested.
    type Birds = Int
    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Option[Pole] =
        if (math.abs((left + n) - right) < 4) copy(left = left + n).some
        else none
      def landRight(n: Birds): Option[Pole] =
        if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
        else none
    }
    S $ (Monad[Option].point(Pole(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)})
    S $ (Monad[Option].point(Pole(0, 0)) >>= { x =>
      x.landRight(2) >>= { y =>
        y.landLeft(2) >>= { z =>
          z.landRight(2)
        }}})

   // LYAHFGG: The second law (right identity) states that if we have a monadic value
    // and we use >>= to feed it to return, the result is our original monadic value.
    S $ (("move on up".some flatMap {Monad[Option].point(_)}) assert_=== "move on up".some)

    // LYAHFGG: The first monad law (left identity) states that if we take a value,
    // put it in a default context with return and then feed it to a function by using >>=,
    // it’s the same as just taking the value and applying the function to it.
    // |> - apply function to self
    S $ ((Monad[Option].point(3) >>= { x => (x + 100000).some }) assert_=== 3 |> { x => (x + 100000).some })
  }

  { // http://eed3si9n.com/learning-scalaz/A-knights-quest.html
    case class KnightPos(c: Int, r: Int) {
      def move: List[KnightPos] =
        for {
          KnightPos(c2, r2) <- List(KnightPos(c + 2, r - 1), KnightPos(c + 2, r + 1),
            KnightPos(c - 2, r - 1), KnightPos(c - 2, r + 1),
            KnightPos(c + 1, r - 2), KnightPos(c + 1, r + 2),
            KnightPos(c - 1, r - 2), KnightPos(c - 1, r + 2)) if (
          ((1 |-> 8) contains c2) && ((1 |-> 8) contains r2))
        } yield KnightPos(c2, r2)

      def in3: List[KnightPos] =
        for {
          first <- move
          second <- first.move
          third <- second.move
        } yield third

      def canReachIn3(end: KnightPos): Boolean = in3 contains end
    }

    S $ (KnightPos(6, 2) canReachIn3 KnightPos(6, 1))
    S $ (KnightPos(6, 2) canReachIn3 KnightPos(7, 3))

    S $ KnightPos(6, 2).move
    S $ KnightPos(8, 1).move
  }

  { // http://eed3si9n.com/learning-scalaz/MonadPlus.html
    // Monad plus also adds filtering
    S $ ((1 |-> 50) filter {x => x.shows contains '7'})

    // Similar to Semigroup[A] and Monoid[A], Plus[F[_]] and PlusEmpty[F[_]] requires
    // their instances to implement plus and empty, but at the type constructor ( F[_]) level.
    S $ List(1, 2, 3) <+> List(4, 5, 6) // appends two monad containers
    S $ List(1, 2, 3) |+| List (4, 5, 6) // appends two monoids
  }

  { // For-comprehensions syntax
    // LYAHFGG: Monads in Haskell are so useful that they got their own special syntax called do notation.
    // Scalaz: Use for-comprehensions

    val monadPlus = for {
      x <- 1 |-> 50 if x.shows contains '7'
    } yield x
    S $ monadPlus

    val l = for {
      n <- List(1, 2)
      ch <- List('a', 'b')
    } yield (n, ch)
    S $ l     // List((1,a), (1,b), (2,a), (2,b))

    // LYAHFGG: When pattern matching fails in a do expression, the fail function is called.
    // It’s part of the Monad type class and it enables failed pattern matching to result
    // in a failure in the context of the current monad instead of making our program crash.
    def wopwop: Option[Char] =
      for {
        (x :: xs) <- "".toList.some
      } yield x
    S $ wopwop

    // LYAHFGG: In do notation, when we bind monadic values to names, we can utilize pattern matching,
    // just like in let expressions and function parameters.
    def justH: Option[Char] =
      for {
        (x :: xs) <- "hello".toList.some
      } yield x
    S $ justH

    // Instead of the do notation in Haskell, Scala has for syntax, which does the same thing:
    val x = for {
      x <- 3.some
      y <- "!".some
    } yield (x.shows + y)
    S $ x

    S $ (3.some >>= { x => (none: Option[String]) >>= { y => (x.shows + y).some } })
    S $ (3.some >>= { x => "!".some >>= { y => (x.shows + y).some } })
  }

  { // http://eed3si9n.com/learning-scalaz/Walk+the+line.html
    type Birds = Int
    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Option[Pole] =
        if (math.abs((left + n) - right) < 4) copy(left = left + n).some
        else none
      def landRight(n: Birds): Option[Pole] =
        if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
        else none
      def banana: Option[Pole] = none
    }

    // Routine with banana
    def routineBanana: Option[Pole] =
      for {
        start <- Monad[Option].point(Pole(0, 0))
        first <- start.landLeft(2)
        _ <- none: Option[Pole]
        second <- first.landRight(2)
        third <- second.landLeft(1)
      } yield third
    S $ routineBanana

    // Our tightwalker’s routine can also be expressed with do notation.
    def routine: Option[Pole] =
      for {
        start <- Monad[Option].point(Pole(0, 0))
        first <- start.landLeft(2)
        second <- first.landRight(2)
        third <- second.landLeft(1)
      } yield third
    S $ routine

    // LYAHFGG: Instead of making functions that ignore their input and
    // just return a predetermined monadic value, we can use the >> function.
    S $ ((none: Option[Int]) >> 3.some)
    S $ ((none: Option[Int]) >>= {x => 3.some}) // equivalent to >>, but easier to code
    S $ (1.some >> 3.some)
    S $ (1.some >>= {x => 3.some}) // equivalent to >>, but easier to code

    // LYAHFGG: We may also devise a function that ignores the current number of birds on the balancing pole
    // and just makes Pierre slip and fall. We can call it banana.
    S $ (Monad[Option].point(Pole(0, 0)) >>= {_.landLeft(1)} >>= {_.banana} >>= {_.landRight(1)})

    S $ (Monad[Option].point(Pole(0, 0)) >>= {_.landLeft(1)} >>= {_.landRight(4)} >>= {_.landLeft(-1)} >>= {_.landRight(-2)})
    S $ (Monad[Option].point(Pole(0, 0)) flatMap {_.landRight(2)} flatMap {_.landLeft(2)} flatMap {_.landRight(2)})
    S $ ((none: Option[Pole]) flatMap {_.landLeft(2)})
    S $ (Pole(0, 0).landRight(1) flatMap {_.landLeft(2)})

    S $ Pole(0, 0).landLeft(2)
    S $ Pole(1, 2).landRight(1)
    S $ Pole(0, 3).landLeft(10)
    // S $ Pole(0, 0).landLeft(1).landRight(1).landLeft(2)
  }

  {
    // Unlike Haskell, Monad[F[_]] exntends Applicative[F[_]]
    // so there’s no return vs pure issues. They both use point.
    S $ Monad[Option].point("What")
    S $ (9.some flatMap {x => Monad[Option].point(x * 10)})

    // Monads bring Bind, whose main operation is flatMap.
    // We're already used to flatMap from the standard Scala library:
    S $ (3.some flatMap {x => (x + 1).some})
    S $ ((none: Option[Int]) flatMap {x => (x + 1).some})
  }

}
