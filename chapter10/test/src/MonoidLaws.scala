import org.scalatest.matchers.should.Matchers.shouldBe

import MonoidSyntax.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Arbitrary
import org.scalatest.funspec.AnyFunSpecLike

/*
Multiple context bounds are separated by ':', like
  A: Arbitrary : Monoid

Multiple using are separated by ',', like
  (using instanceM: Monoid[A], arbA: Arbitrary[A]).
If one using parameter is named, all should be named?
 */
trait MonoidLaws extends AnyFunSpecLike with ScalaCheckPropertyChecks:
  def leftIdentity[A: Arbitrary: Monoid]: Unit =
    it("should satisfy the left identity law"):
      forAll { (x: A) =>
        (summon[Monoid[A]].empty |+| x) shouldBe x
      }

  def rightIdentity[A: Arbitrary: Monoid]: Unit =
    it("should satisfy the right identity law"):
      forAll { (x: A) =>
        (x |+| summon[Monoid[A]].empty) shouldBe x
      }

  def associativity[A: Arbitrary: Monoid]: Unit =
    it("should satisfy the associativity law"):
      forAll { (x: A, y: A, z: A) =>
        ((x |+| y) |+| z) shouldBe (x |+| (y |+| z))
      }
