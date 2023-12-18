import Lib.*

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class LibSpec extends AnyFunSpec:
  val rng = SimpleRNG(42)

  describe("Chapter 6"):
    it("nonNegativeInt should generate a random non-negative integer"):
      1 to 1000 foreach { _ =>
        val (n, _) = nonNegativeInt(rng)
        n >= 0 && n <= Int.MaxValue shouldBe true
      }

    it("double should generate a non-negative double"):
      1 to 1000 foreach { _ =>
        val (n, _) = double(rng)
        n >= 0 && n < 1 shouldBe true
      }

    it("ints should generate as many non-negative integers are the given count"):
      val count   = 3
      val (is, r) = ints(count)(rng)
      is.size shouldBe count
      is.forall(_ >= 0) shouldBe true
