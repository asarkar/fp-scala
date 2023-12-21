import org.scalacheck.{Arbitrary, Gen}
class OptionApplicativeLawsSpec extends ApplicativeLaws[Option]("Option applicative"):
  import ApplicativeInstances.OptionApplicative

  checkAll[Int, Int, Int]

class ValidApplicativeLawsSpec extends ApplicativeLaws[Validated[NonEmptyList[String], _]]("Validated applicative"):
  import ApplicativeInstances.validatedApplicative
  import SemigroupInstances.nelSemigroup
  import Validated.*

  val genNonNegativeInt                 = Gen.choose(0, 10)
  val genPostiveInt                     = Gen.choose(1, 100)
  def genStrN(n: Int): Gen[String]      = Gen.stringOfN(n, Gen.asciiPrintableChar)
  def genNel(n: Int): Gen[List[String]] = Gen.listOfN(n, genNonNegativeInt.flatMap(genStrN))
  val genNEL: Gen[NonEmptyList[String]] = genPostiveInt
    .flatMap(n => genNel(n).flatMap(xs => NonEmptyList(xs.head, xs.tail)))
  val genValidated = Gen.oneOf(
    genNEL.flatMap(Invalid(_)),
    genNonNegativeInt.flatMap(Valid(_))
  )

  given arbValidated: Arbitrary[Validated[NonEmptyList[String], Int]] =
    Arbitrary(genValidated)

  checkAll[Int, Int, Int]
