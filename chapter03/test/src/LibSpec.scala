import org.scalatest.funspec.AnyFunSpec
import Lib.*
import org.scalatest.matchers.should.Matchers.shouldBe

class LibSpec extends AnyFunSpec:
  describe("Chapter 3"):
    it("list pattern match should add first two values"):
      result shouldBe 3
