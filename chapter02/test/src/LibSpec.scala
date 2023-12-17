import org.scalatest.funspec.AnyFunSpec
import Lib.*
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.Table
import org.scalatest.matchers.should.Matchers.shouldBe

class LibSpec extends AnyFunSpec with TableDrivenPropertyChecks:
  describe("Chapter 2"):
    it("fib should return the nth Fibonacci number"):
      val fst20 = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181)
      for ((expected, n) <- fst20.zipWithIndex)
        fib(n) shouldBe expected

    val isSortedInput =
      Table(
        ("as", "gt", "expected"),
        (Array(1, 2, 3), (_: Int) > (_: Int), true),
        (Array(1, 2, 1), (_: Int) > (_: Int), false),
        (Array(3, 2, 1), (_: Int) < (_: Int), true),
        (Array(1, 2, 3), (_: Int) < (_: Int), false)
      )

    it("isSorted should check if an array is sorted"):
      forAll(isSortedInput) { (as: Array[Int], gt: (Int, Int) => Boolean, expected: Boolean) =>
        isSorted(as, gt) shouldBe expected
      }

    it("curry should convert a two-argument function into an one-argument function that returns a function"):
      val add = (x: Int, y: Int) => x + y
      curry(add)(1)(2) shouldBe 3

    it("uncurry should reverse curry"):
      val curriedAdd = (a: Int) => (b: Int) => a + b
      uncurry(curriedAdd)(1, 2) shouldBe 3

    it("compose should compose two functions"):
      val plus2  = (x: Int) => x + 2
      val times2 = (x: Int) => x * 2
      compose(times2, plus2)(2) shouldBe 8
