import LazyList.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

/*
LazyList instances can't be compared because
it uses anonymous lambdas for elements. The
elements must be evaluated before comparison.
 */
class LazyListSpec extends AnyFunSpec:
  describe("LazyList"):
    it("toList should convert it to a Scala list"):
      LazyList(1, 2, 3).toList shouldBe List(1, 2, 3)

    it("take should take the first n element"):
      LazyList(1, 2, 3).take(2).toList shouldBe List(1, 2)

    it("take should return an empty list when invoked on an empty list"):
      LazyList.empty[Int].take(2).toList shouldBe List.empty[Int]

    it("take should return an the same list when n is greater than the length of the list"):
      LazyList(1).take(2).toList shouldBe List(1)

    it("drop should drop the first n element"):
      LazyList(1, 2, 3).drop(2).toList shouldBe List(3)

    it("drop should return an empty list when all elements are dropped"):
      LazyList(1, 2, 3).drop(3).toList shouldBe List.empty[Int]

    it("forAll should check if all elements match the given predicate"):
      LazyList(1, 2, 3).forAll(_ % 2 == 0) shouldBe false

    it("takeWhile should return all starting elements that match the given predicate"):
      LazyList(1, 2, 3).takeWhile(_ % 2 == 1).toList shouldBe List(1)
      LazyList(1, 2, 3).takeWhile(_ % 2 == 0).toList shouldBe List.empty

    it("headOption should return Some(head) if the list is not empty"):
      LazyList(1, 2).headOption shouldBe Some(1)

    it("headOption should return None if the list is empty"):
      LazyList.empty.headOption shouldBe None

    it("map should transform a non empty list"):
      LazyList(1, 2).map(_ * 2).toList shouldBe List(2, 4)

    it("map should have no effect on an empty list"):
      LazyList.empty[Int].map(_ * 2).toList shouldBe List.empty

    it("filter should remove the elements that don't satisfy the given predicate"):
      LazyList(1, 2, 3).filter(_ % 2 == 0).toList shouldBe List(2)

    it("filter should have no effect on an empty list"):
      LazyList.empty[Int].filter(_ % 2 == 0).toList shouldBe List.empty

    it("append should append the other list at the end"):
      LazyList(1, 2).append(LazyList(3, 4)).toList shouldBe List(1, 2, 3, 4)
      LazyList.empty.append(LazyList(3, 4)).toList shouldBe List(3, 4)

    it("flatMap should replace each element with a list and flatten the final list"):
      LazyList(1, 2).flatMap(a => LazyList(a, a)).toList shouldBe List(1, 1, 2, 2)

    it("continually should generate an infinite stream of the given value"):
      continually(1).take(3).toList shouldBe List(1, 1, 1)

    it("from should generate an infinite stream starting with the given value"):
      from(1).take(3).toList shouldBe List(1, 2, 3)

    it("fibs should generate an infinite stream of Fibonacci numbers"):
      fibs.take(20).toList shouldBe
        List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181)

    it("onesViaUnfold should generate an infinite stream of ones"):
      onesViaUnfold.take(10).toList shouldBe List.fill(10)(1)

    it("fibsViaUnfold should generate an infinite stream of Fibonacci numbers"):
      fibsViaUnfold.take(20).toList shouldBe
        List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181)

    it("fromViaUnfold should generate an infinite stream starting with the given value"):
      fromViaUnfold(1).take(3).toList shouldBe List(1, 2, 3)

    it("continuallyViaUnfold should generate an infinite stream of the given value"):
      continuallyViaUnfold(1).take(3).toList shouldBe List(1, 1, 1)

    it("mapViaUnfold should transform a non empty list"):
      LazyList(1, 2).mapViaUnfold(_ * 2).toList shouldBe List(2, 4)

    it("mapViaUnfold should have no effect on an empty list"):
      LazyList.empty[Int].mapViaUnfold(_ * 2).toList shouldBe List.empty

    it("takeViaUnfold should take the first n element"):
      LazyList(1, 2, 3).takeViaUnfold(2).toList shouldBe List(1, 2)

    it("takeViaUnfold should return an empty list when invoked on an empty list"):
      LazyList.empty[Int].takeViaUnfold(2).toList shouldBe List.empty[Int]

    it("takeViaUnfold should return an the same list when n is greater than the length of the list"):
      LazyList(1).takeViaUnfold(2).toList shouldBe List(1)

    it("takeWhileViaUnfold should return all starting elements that match the given predicate"):
      LazyList(1, 2, 3).takeWhileViaUnfold(_ % 2 == 1).toList shouldBe List(1)
      LazyList(1, 2, 3).takeWhileViaUnfold(_ % 2 == 0).toList shouldBe List.empty

    it("zipWith should combine elements from the given lists pairwise using the given function"):
      LazyList(1, 2, 3)
        .zipWith(
          LazyList(true, false, true),
          (x, y) => if y then x.toString() else y.toString()
        )
        .toList shouldBe List("1", "false", "3")

    it("zipWith should terminate with the smaller list"):
      LazyList(1, 2, 3)
        .zipWith(
          LazyList(true),
          (x, y) => if y then x.toString() else y.toString()
        )
        .toList shouldBe List("1")

    it("zipAll should continue as long as any one list is not empty"):
      LazyList(1, 2, 3).zipAll(LazyList(true)).toList shouldBe
        List(Some(1) -> Some(true), Some(2) -> None, Some(3) -> None)

    it("startsWith should check if the given list is a prefix of this list"):
      LazyList(1, 2, 3).startsWith(LazyList(1, 2)) shouldBe true
      LazyList(1).startsWith(LazyList(1, 2)) shouldBe false
      LazyList(1, 3, 2).startsWith(LazyList(1, 2)) shouldBe false

    it("tails should return all the suffixes of the list"):
      LazyList(1, 2, 3).tails.map(_.toList).toList shouldBe
        List(List(1, 2, 3), List(2, 3), List(3), List())

    it("scanRight should return a lazy list of the intermediate results"):
      LazyList(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
