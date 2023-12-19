import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class MonoidSpec extends AnyFunSpec:
  describe("Monoid"):
    it("foldLeft should fold correctly"):
      import MonoidInstances.stringMonoid
      Monoid.foldLeft(List("a", "b", "c"))("")(_ + _) shouldBe "abc"

    it("foldRight should fold correctly"):
      import MonoidInstances.stringMonoid
      Monoid.foldRight(List("a", "b", "c"))("")(_ + _) shouldBe "abc"
