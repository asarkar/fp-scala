import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

/*
Exercise 10.4: Use a property-based testing
framework to test the monoids we've written.
 */
class MonoidLawsSpec extends AnyFunSpec with ScalaCheckPropertyChecks with MonoidLaws:
  describe("Integers form a Monoid under addition"):
    import MonoidInstances.intAddition

    associativityLaw[Int]
    identityLaw[Int]

  describe("Integers form a Monoid under multiplication"):
    given Monoid[Int] = MonoidInstances.intMultiplication

    associativityLaw[Int]
    identityLaw[Int]

  describe("Booleans form a Monoid under disjunction (OR)"):
    given Monoid[Boolean] = MonoidInstances.booleanOr

    associativityLaw[Boolean]
    identityLaw[Boolean]

  describe("Booleans form a Monoid under conjunction (AND)"):
    given Monoid[Boolean] = MonoidInstances.booleanAnd

    associativityLaw[Boolean]
    identityLaw[Boolean]

  describe("Options form a Monoid"):
    given Monoid[Option[Int]] = MonoidInstances.optionMonoid

    associativityLaw[Option[Int]]
    identityLaw[Option[Int]]

  import MonoidInstances.WC

  val wcGen: Gen[WC] =
    def genStringN(n: Int) = Gen
      .containerOfN[List, Char](n, Gen.asciiPrintableChar)
      .flatMap(_.mkString)
    val genString = Gen.choose(0, 10).flatMap(genStringN)
    val genStub   = genString.map(WC.Stub(_))
    val genPart = for
      lStub <- genString
      words <- Gen.choose(0, 10)
      rStub <- genString
    yield WC.Part(lStub, words, rStub)

    Gen.oneOf(genStub, genPart)

  describe("WC forms a Monoid"):
    import MonoidInstances.WC

    given Arbitrary[WC] = Arbitrary(wcGen)
    given Monoid[WC]    = MonoidInstances.wcMonoid

    associativityLaw[WC]
    identityLaw[WC]

  describe("Tuple2s form a Monoid under integer addition"):
    import MonoidInstances.{productMonoid, intAddition}

    associativityLaw[(Int, Int)]
    identityLaw[(Int, Int)]

  describe("Tuple2s form a Monoid under integer multiplication"):
    import MonoidInstances.productMonoid
    given Monoid[Int] = MonoidInstances.intMultiplication

    associativityLaw[(Int, Int)]
    identityLaw[(Int, Int)]

  describe("Maps form a Monoid on their values"):
    import MonoidInstances.{mapMergeMonoid, intAddition}

    associativityLaw[Map[String, Int]]
    identityLaw[Map[String, Int]]
