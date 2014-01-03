package scalazg.learning.day6

import scalaz._
import Scalaz._
import scalazg._
import std.vector.vectorSyntax

/**
 * http://eed3si9n.com/learning-scalaz/Writer.html
 */
object WriterMonads extends App {

  {
    // Microbenchmark to compare Vector vs List
    def vectorFinalCountDown(x: Int): Writer[Vector[String], Unit] = {
      import annotation.tailrec
      @tailrec def doFinalCountDown(x: Int, w: Writer[Vector[String], Unit]): Writer[Vector[String], Unit] = x match {
        case 0 => w >>= { _ => Vector("0").tell }
        case x => doFinalCountDown(x - 1, w >>= { _ =>
          Vector(x.shows).tell
        })
      }
      val t0 = System.currentTimeMillis
      val r = doFinalCountDown(x, Vector[String]().tell)
      val t1 = System.currentTimeMillis
      r >>= { _ => Vector((t1 - t0).shows + " msec").tell }
    }

    def listFinalCountDown(x: Int): Writer[List[String], Unit] = {
      import annotation.tailrec
      @tailrec def doFinalCountDown(x: Int, w: Writer[List[String], Unit]): Writer[List[String], Unit] = x match {
        case 0 => w >>= { _ => List("0").tell }
        case x => doFinalCountDown(x - 1, w >>= { _ =>
          List(x.shows).tell
        })
      }
      val t0 = System.currentTimeMillis
      val r = doFinalCountDown(x, List[String]().tell)
      val t1 = System.currentTimeMillis
      r >>= { _ => List((t1 - t0).shows + " msec").tell }
    }

    // S $ listFinalCountDown(10000).run._1.last
    // S $ vectorFinalCountDown(10000).run._1.last
  }

  {
    // LYAHFGG: When using the Writer monad, you have to be careful which monoid to use,
    // because using lists can sometimes turn out to be very slow. That’s because lists
    // use ++ for mappend and using ++ to add something to the end of a list is slow if
    // that list is really long.
    // Scalaz: Here's the table of performance characteristics for major collections:
    // http://docs.scala-lang.org/overviews/collections/performance-characteristics.html
    // What stands out for immutable collection is Vector since it has effective
    // constant for all operations. Vector is a tree structure with the branching factor
    // of 32, and it’s able to achieve fast updates by structure sharing.

    // Adding logging to program
    def gcd(a: Int, b: Int): Writer[Vector[String], Int] =
      if (b == 0) {
        for {
          _ <- Vector(s"Finished with ${a.shows}").tell
        } yield a
      } else {
        for {
          result <- gcd(b, a % b)
          _ <- Vector(s"${a.shows} mod ${b.shows} = ${(a % b).shows}").tell
        } yield result
      }
    S $ gcd(8, 3).run

    // Adding logging to program
    def gcdList(a: Int, b: Int): Writer[List[String], Int] =
      if (b == 0) {
        for {
          _ <- List(s"Finished with ${a.shows}").tell
        } yield a
      } else {
        List(s"${a.shows} mod ${b.shows} = ${(a % b).shows}").tell >>= { _ => gcdList(b, a % b)}
      }
    S $ gcdList(8, 3).run

    // LYAHFGG: Now that we have a Monad instance, we’re free to use do notation for Writer values.
    def logNumber(x: Int): Writer[List[String], Int] = x.set(List(s"Got number: $x"))
    def multWithLog: Writer[List[String], Int] = for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b // Just multiply, log messages are appended via map/flatmap operations of Writer
    // S $ multWithLog -> must call `run` to actually execute stuff, otherwise just returns the String version
    S $ multWithLog.run

    // Construct a writer
    S $ 3.set("Smallish gang.")

    def isBigGang(x: Int): (Boolean, String) = (x > 9, "Compared gang size to 9.")
    // More generic version to log using Monoids
    implicit class PairOps[A, B: Monoid](pair: (A, B)) {
      def applyLog[C](f: A => (C, B)): (C, B) = {
        val (x, log) = pair
        val (y, newlog) = f(x)
        (y, log |+| newlog)
      }
    }
    // Conversion to log using Strings
    implicit class PairStringOps[A](pair: (A, String)) {
      def applyLogString[B](f: A => (B, String)): (B, String) = {
        val (x, log) = pair
        val (y, newlog) = f(x)
        (y, log ++ newlog)
      }
    }

    // LYAHFGG: Writer monad is for values that have another value attached that acts as a sort of log value.
    // To attach a monoid to a value, we just need to put them together in a tuple.
    // The Writer w a type is just a newtype wrapper for this.

    S $ ((3, "Smallish gang.") applyLog isBigGang)
    S $ ((3, "Smallish gang.") applyLogString isBigGang)
  }

}
