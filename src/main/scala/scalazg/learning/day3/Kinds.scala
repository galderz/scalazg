package scalazg.learning.day3

import scalaz._
import Scalaz._
import scalazg._
import scala.util.Try

/**
 * http://eed3si9n.com/learning-scalaz/
 */
object Kinds extends App {

  {
    // In case of Scalaz 7, Equal and others have the kind * -> * while Functor and
    // all its derivatives have the kind (* -> *) -> *. You wouldn’t worry about this
    // if you are using injected operators like:
    S $ List(1, 2, 3).shows

    // But if you want to use Show[A].shows, you have to know it’s Show[List[Int]], not Show[List].
    // Similarly, if you want to lift a function, you need to know that it’s Functor[F] (F is for Functor):
    // Functor[List[Int]].lift((_: Int) + 2) -> Error: List[Int] takes no type parameters, expected: one
    def f = Functor[List].lift((_: Int) + 2)
    S $ f(List(1, 2, 3))

    // From the top. Int and every other types that you can make a value out of is called a proper type and
    // denoted with a symbol * (read “type”). This is analogous to value 1 at value-level.
    S $ kind[Int]           // Int's kind is *. This is a proper type.

    // A first-order value, or a value constructor like (_: Int) + 3, is normally called a function.
    // Similarly, a first-order-kinded type is a type that accepts other types to create a proper type.
    // This is normally called a type constructor. Option, Either, and Equal are all first-order-kinded.
    // To denote that these accept other types, we use curried notation like * -> * and * -> * -> *.
    // Note, Option[Int] is *; Option is * -> *.
    S $ kind[Option.type]   // Option's kind is * -> *. This is a type constructor: a 1st-order-kinded type.
    S $ kind[Either.type]   // Either's kind is * -> * -> *. This is a type constructor: a 1st-order-kinded type.
    S $ kind[Try.type]      // Try's kind is * -> *. This is a type constructor: a 1st-order-kinded type.
    S $ kind[Equal.type]    // Equal's kind is * -> *. This is a type constructor: a 1st-order-kinded type.

    // A higher-order value like (f: Int => Int, list: List[Int]) => list map {f}, a function that accepts other
    // functions is normally called higher-order function. Similarly, a higher-kinded type is a type constructor
    // that accepts other type constructors. It probably should be called a higher-kinded type constructor but
    // the name is not used. These are denoted as (* -> *) -> *.
    S $ kind[Functor.type]  // Functor's kind is (* -> *) -> *. This is a type constructor that takes type constructor(s): a higher-kinded type.

    /**
     * Types are little labels that values carry so that we can reason about the values.
     * But types have their own little labels, called kinds. A kind is more or less the type of a type. …
     * What are kinds and what are they good for? Well, let’s examine the kind of a type by using
     * the :k command in GHCI. The kind method behaves just like :k in Haskell's GHCI.
     */
    def kind[A: scala.reflect.runtime.universe.TypeTag]: String = {
      import scala.reflect.runtime.universe._
      def typeKind(sig: Type): String = sig match {
        case PolyType(params, resultType) =>
          (params map { p =>
            typeKind(p.typeSignature) match {
              case "*" => "*"
              case s   => "(" + s + ")"
            }
          }).mkString(" -> ") + " -> *"
        case _ => "*"
      }
      def typeSig(tpe: Type): Type = tpe match {
        case SingleType(pre, sym) => sym.companionSymbol.typeSignature
        case ExistentialType(q, TypeRef(pre, sym, args)) => sym.typeSignature
        case TypeRef(pre, sym, args) => sym.typeSignature
      }
      val sig = typeSig(typeOf[A])
      val s = typeKind(sig)
      sig.typeSymbol.name + "'s kind is " + s + ". " + (s match {
        case "*" =>
          "This is a proper type."
        case x if !(x contains "(") =>
          "This is a type constructor: a 1st-order-kinded type."
        case x =>
          "This is a type constructor that takes type constructor(s): a higher-kinded type."
      })
    }
  }

}
