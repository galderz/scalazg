package scalazg.learning.day3

import scalaz._
import Scalaz._
import scalazg._

/**
 * http://eed3si9n.com/learning-scalaz/
 */
object TaggedTypes extends App {

  {
    sealed trait JoulePerKiloGram
    def JoulePerKiloGram[A](a: A): A @@ JoulePerKiloGram = Tag[A, JoulePerKiloGram](a)
    def energyR(m: Double @@ KiloGram): Double @@ JoulePerKiloGram = JoulePerKiloGram(299792458.0 * 299792458.0 * m)
    S $ energyR(KiloGram(20.0))   // scalaz.@@[Double,JoulePerKiloGram] = 1.79751035747363533E18
    // S $ energyR(10.0) -> Error: type mismatch; found   : Double(10.0) required: scalaz.@@[Double,KiloGram]

    // Suppose we want a way to express mass using kilogram, because kg is the international standard of unit.
    // Normally we would pass in Double and call it a day, but we can’t distinguish that from other Double values.
    // Can we use case class for this?
    // One option is using case classes, e.g. case class KiloGram(value: Double)
    // Although it does adds type safety, it’s not fun to use because we have to call x.value
    // every time we need to extract the value out of it. Tagged type to the rescue.
    sealed trait KiloGram
    def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)
    S $ KiloGram(20.0)      // scalaz.@@[Double,KiloGram] = 20.0
    S $ 2 * KiloGram(20.0)  // Double = 40.0

    // LYAHFGG: The newtype keyword in Haskell is made exactly for these cases when
    // we want to just take one type and wrap it in something to present it as another type.
  }

}
