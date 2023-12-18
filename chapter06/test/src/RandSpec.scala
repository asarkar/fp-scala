import Rand.*
import Lib.nonNegativeInt

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class RandSpec extends AnyFunSpec:
  val rng = SimpleRNG(42)

  describe("Rand"):
    it("ints should generate as many non-negative integers are the given count"):
      val count   = 3
      val (is, r) = intsViaSequence(count)(nonNegativeInt)(rng)
      is.size shouldBe count
      is.forall(_ >= 0) shouldBe true
