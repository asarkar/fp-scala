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

    leftIdentity[Int]
    rightIdentity[Int]
    associativity[Int]

  describe("Integers form a Monoid under multiplication"):
    given Monoid[Int] = MonoidInstances.intMultiplication

    leftIdentity[Int]
    rightIdentity[Int]
    associativity[Int]

  describe("Booleans form a Monoid under disjunction (OR)"):
    given Monoid[Boolean] = MonoidInstances.booleanOr

    leftIdentity[Boolean]
    rightIdentity[Boolean]
    associativity[Boolean]

  describe("Booleans form a Monoid under conjunction (AND)"):
    given Monoid[Boolean] = MonoidInstances.booleanAnd

    leftIdentity[Boolean]
    rightIdentity[Boolean]
    associativity[Boolean]

  describe("Option forms a Monoid"):
    given Monoid[Option[Int]] = MonoidInstances.optionMonoid

    leftIdentity[Option[Int]]
    rightIdentity[Option[Int]]
    associativity[Option[Int]]

  import MonoidInstances.WC

  val wcGen: Gen[WC] =
    val genString = Gen
      .choose(0, 10)
      .flatMap(Gen.stringOfN(_, Gen.asciiPrintableChar))
    val genStub = genString.map(WC.Stub(_))
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

    leftIdentity[WC]
    rightIdentity[WC]
    associativity[WC]

  describe("Tuple2s form a Monoid under integer addition"):
    import MonoidInstances.{productMonoid, intAddition}

    leftIdentity[(Int, Int)]
    rightIdentity[(Int, Int)]
    associativity[(Int, Int)]

  describe("Tuple2s form a Monoid under integer multiplication"):
    import MonoidInstances.productMonoid
    given Monoid[Int] = MonoidInstances.intMultiplication

    leftIdentity[(Int, Int)]
    rightIdentity[(Int, Int)]
    associativity[(Int, Int)]

  describe("Maps form a Monoid on their values"):
    import MonoidInstances.{mapMergeMonoid, intAddition}

    leftIdentity[Map[String, Int]]
    rightIdentity[Map[String, Int]]
    associativity[Map[String, Int]]
