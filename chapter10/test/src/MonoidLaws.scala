import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

import MonoidSyntax.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Arbitrary

/*
Multiple context bounds are separated by ':', like
  A: Arbitrary : Monoid

Multiple using are separated by ',', like
  (using instanceM: Monoid[A], arbA: Arbitrary[A]).
If one using parameter is named, all should be named?
 */
trait MonoidLaws { this: AnyFunSpec & ScalaCheckPropertyChecks =>
  def associativityLaw[A: Arbitrary: Monoid]: Unit =
    it("should satisfy the associativity law"):
      forAll { (x: A, y: A, z: A) =>
        ((x |+| y) |+| z) shouldBe (x |+| (y |+| z))
      }

  def identityLaw[A: Arbitrary: Monoid]: Unit =
    val instanceM = summon[Monoid[A]]

    it("should satisfy the identity law"):
      forAll { (x: A) =>
        (x |+| instanceM.empty) shouldBe x
        (instanceM.empty |+| x) shouldBe x
      }
}
