import org.scalatest.funspec.AnyFunSpec
import Lib.*

class LibSpec extends AnyFunSpec:
  describe("Chapter 2"):
    it("fib should return the nth Fibonacci number"):
      val fst20 = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181)
      for ((expected, n) <- fst20.zipWithIndex)
        assert(fib(n) == expected, s"for n=$n")

    it("isSorted should check if an array is sorted"):
      assert(isSorted(Array(1, 2, 3), _ > _))
      assert(!isSorted(Array(1, 2, 1), _ > _))
      assert(isSorted(Array(3, 2, 1), _ < _))
      assert(!isSorted(Array(1, 2, 3), _ < _))

    it("curry should convert a two-argument function into an one-argument function that returns a function"):
      val add = (x: Int, y: Int) => x + y
      assert(curry(add)(1)(2) == 3)

    it("uncurry should reverse curry"):
      val curriedAdd = (a: Int) => (b: Int) => a + b
      assert(uncurry(curriedAdd)(1, 2) == 3)

    it("compose should compose two functions"):
      val plus2  = (x: Int) => x + 2
      val times2 = (x: Int) => x * 2
      assert(compose(times2, plus2)(2) == 8)
