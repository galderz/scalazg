package scalazg.learning.day3

import scalaz._
import Scalaz._
import scalazg._

/**
 * http://eed3si9n.com/learning-scalaz/
 */
object Monoids extends App {

  {
    // LYAHFGG: OK, so how is this monoid useful? Let’s say you were writing a function that takes two strings,
    // compares their lengths, and returns an Ordering. But if the strings are of the same length,
    // then instead of returning EQ right away, we want to compare them alphabetically.
    def lengthCompare(lhs: String, rhs: String): Ordering = (lhs.length cmp rhs.length) |+| (lhs cmp rhs)
    S $ lengthCompare("zen", "ants")      // scalaz.Ordering = LT
    S $ lengthCompare("zen", "ant")       // scalaz.Ordering = GT

    // LYAHFGG: With Ordering, we have to look a bit harder to recognize a monoid,
    // but it turns out that its Monoid instance is just as intuitive as the ones
    // we’ve met so far and also quite useful.
    // S $ (Ordering.LT |+| Ordering.GT) -> Error: value |+| is not a member of object scalaz.Ordering.LT
    S $ (Ordering.LT: Ordering) |+| (Ordering.GT: Ordering)       // scalaz.Ordering = LT
    S $ (Ordering.GT: Ordering) |+| (Ordering.LT: Ordering)       // scalaz.Ordering = GT
    S $ Monoid[Ordering].zero |+| (Ordering.GT: Ordering)       // scalaz.Ordering = GT
    S $ Monoid[Ordering].zero |+| (Ordering.LT: Ordering)       // scalaz.Ordering = LT

    // LYAHFGG: Another type which can act like a monoid in two distinct but equally valid ways is Bool.
    // The first way is to have the or function || act as the binary function along with False as the identity value. …
    // The other way for Bool to be an instance of Monoid is to kind of do the opposite:
    // have && be the binary function and then make True the identity value.
    // Scalaz: || is Tags.Disjunction, && is Tags.Conjunction
    S $ (Tags.Disjunction(true) |+| Tags.Disjunction(false))                    // true = true || false
    S $ (Monoid[Boolean @@ Tags.Disjunction].zero |+| Tags.Disjunction(true))   // true = true || false
    S $ (Monoid[Boolean @@ Tags.Conjunction].zero |+| Tags.Conjunction(true))   // true = true && true
    S $ (Monoid[Boolean @@ Tags.Conjunction].zero |+| Tags.Conjunction(false))  // false = true && false

    // LYAHFGG: So now that there are two equally valid ways for numbers (addition and multiplication)
    // to be monoids, which way do choose? Well, we don’t have to.
    // Scalaz: This is where Scalaz 7 uses tagged type.
    S $ Monoid[Int @@ Tags.Multiplication].zero                                 // 1
    S $ (Tags.Multiplication(10) |+| Monoid[Int @@ Tags.Multiplication].zero)   // 10 = 10 * 1
    // Nice! So we can multiply numbers using |+|. For addition, we use plain Int.
    S $ (10 |+| Monoid[Int].zero)                                               // 10 = 10 + 0

    // LYAHFGG: mempty represents the identity value for a particular monoid.
    // Scalaz: calls this zero
    S $ Monoid[List[Int]].zero    // List[Int] = List()
    S $ Monoid[String].zero       // String = ""

    // LYAHFGG: A monoid is when you have an associative binary function and
    // a value which acts as an identity with respect to that function.
    // LYAHFGG: We have mappend, which, as you’ve probably guessed, is the binary function.
    // It takes two values of the same type and returns a value of that type as well.
    // Scalaz: It introduces `mappend` operator with symbolic alias |+| and ⊹
    S $ (List(1, 2, 3) mappend List (4, 5, 6))
    S $ ("one" |+| "two")

    // LYAHFGG: It doesn’t matter if we do (3 * 4) * 5 or 3 * (4 * 5). Either way, the result is 60.
    // The same goes for ++. … We call this property associativity. * is associative, and so is ++,
    // but -, for example, is not.
    S $ ((3 * 2) * (8 * 5) assert_=== 3 * (2 * (8 * 5)))
    S $ (List("la") ++ (List("di") ++ List("da")) assert_=== (List("la") ++ List("di")) ++ List("da"))
    try {
      S $ ((3 - 2) - (8 - 5) assert_=== 3 - (2 - (8 - 5)))
    } catch {
      case r: RuntimeException => r.printStackTrace() // expected
    }
  }


}
