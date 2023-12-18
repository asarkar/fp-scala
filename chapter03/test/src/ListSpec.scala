import org.scalatest.funspec.AnyFunSpec
import List.*
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.Table
import org.scalatest.matchers.should.Matchers.{shouldBe, should, thrownBy, a, be}
import scala.math.Pi

class ListSpec extends AnyFunSpec with TableDrivenPropertyChecks:
  describe("List"):
    it("tail should remove the first element of a non-empty list"):
      val xs = List(1, 2)
      tail(xs) shouldBe List(2)

    it("tail should throw on an empty list"):
      a[RuntimeException] should be thrownBy tail(List())

    it("setHead should replace the first element"):
      setHead(List(1, 2), 3) shouldBe List(3, 2)

    val dropInput =
      Table(
        ("xs", "n", "expected"),
        (List(1, 2, 3), 2, List(3)),
        (List(1, 2), 2, List()),
        (List(), 1, List()),
        (List(1), 0, List(1))
      )

    it("drop should remove the first n elements"):
      forAll(dropInput) { (xs: List[Int], n: Int, expected: List[Int]) =>
        drop(xs, n) shouldBe expected
      }

    val dropWhileInput =
      Table(
        ("xs", "f", "expected"),
        (List(1, 2, 3), (_: Int) < 3, List(3)),
        (List(1, 2, 3), (_: Int) > 1, List(1, 2, 3)),
        (List(1, 2, 3), (_: Int) => true, List())
      )

    it("dropWhile should remove elements as long as they match the predicate"):
      forAll(dropWhileInput) { (xs: List[Int], f: Int => Boolean, expected: List[Int]) =>
        dropWhile(xs, f) shouldBe expected
      }

    it("init should remove the last element"):
      init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
      init(List(1)) shouldBe List()

    it("init should throw on an empty list"):
      a[RuntimeException] should be thrownBy init(List())

    it("length should return the length of the list"):
      length(List()) shouldBe 0
      length(List(1, 2, 3)) shouldBe 3

    it("sumViaFoldLeft should compute the sum using foldLeft"):
      sumViaFoldLeft(List()) shouldBe 0
      sumViaFoldLeft(List(1, 2, 3)) shouldBe 6

    it("productViaFoldLeft should compute the product using foldLeft"):
      productViaFoldLeft(List()) shouldBe 1
      productViaFoldLeft(List(1, 2, 3)) shouldBe 6

    it("lengthViaFoldLeft should compute the length using foldLeft"):
      lengthViaFoldLeft(List()) shouldBe 0
      lengthViaFoldLeft(List(1, 2, 3)) shouldBe 3

    it("reverse should reverse the list"):
      reverse(List()) shouldBe List()
      reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)

    it("foldRight can be implemented using foldLeft"):
      foldRight(List(8, 12, 24, 4), 2.0, _ / _) shouldBe 8.0

    it("append should concatenate two lists"):
      append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)

    it("concat should flatten a list of lists"):
      concat(List(List(1, 2, 3), List(4, 5), List(6), List())) shouldBe List(1, 2, 3, 4, 5, 6)

    it("add1 should increment every integer element by 1"):
      add1(List(1, 2, 3)) shouldBe List(2, 3, 4)

    it("doubleToString should convert every double element to a string"):
      doubleToString(List(1, 2, Pi)) shouldBe List("1.00", "2.00", "3.14")

    it("map should transform each element using the given function"):
      map(List(1, 2, 3), _ + 1) shouldBe List(2, 3, 4)
      map(List(1, 2, Pi), x => f"$x%2.2f") shouldBe List("1.00", "2.00", "3.14")

    it("filter should remove the elements that don't satisfy the given predicate"):
      filter(List(1, 2, 3), x => x % 2 == 0) shouldBe List(2)

    it("flatMap should replace each element with a list and flatten the final list"):
      flatMap(List(1, 2, 3), i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)

    it("zipWith should combine elements from the given lists pairwise using the given function"):
      zipWith(
        List(1, 2, 3),
        List(true, false, true),
        (x, y) => if y then x.toString() else y.toString()
      ) shouldBe List("1", "false", "3")

    it("zipWith should terminate with the smaller list"):
      zipWith(
        List(1, 2, 3),
        List(true),
        (x, y) => if y then x.toString() else y.toString()
      ) shouldBe List("1")
