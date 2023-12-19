import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import FoldableInstances.{Tree, given}

class FoldableSpec extends AnyFunSpec:
  describe("Foldable"):
    describe("Tree"):
      val intTree = Tree.Branch(Tree.Branch(Tree.Leaf(10), Tree.Leaf(20)), Tree.Leaf(30))

      it("should calculate the average in an integer tree"):
        def avg(total: Int, count: Int) = (total.toDouble / count.toDouble)

        val (total, count) = intTree.foldRight((0, 0))((x, acc) => (acc._1 + x, acc._2 + 1))
        avg(total, count) shouldBe 20.0d

        val (total1, count1) = intTree.foldLeft((0, 0))((acc, x) => (acc._1 + x, acc._2 + 1))
        avg(total1, count1) shouldBe 20.0d

        import MonoidInstances.{productMonoid, intAddition}
        val (total2, count2) = intTree.foldMap(x => (x, 1))
        avg(total2, count2) shouldBe 20.0d

      it("should calculate the sum in an integer tree"):
        val sum = intTree.foldRight(0)(_ + _)
        sum shouldBe 60

        val sum1 = intTree.foldLeft(0)(_ + _)
        sum1 shouldBe 60

      it("should concatenate the words in a string tree"):
        val strTree = Tree.Branch(
          Tree.Branch(Tree.Leaf("my"), Tree.Leaf("dog")),
          Tree.Branch(Tree.Leaf("likes"), Tree.Leaf("summer"))
        )
        val s = strTree.foldRight("")((x, acc) => s"$x $acc")
        s shouldBe "my dog likes summer "

        val s1 = strTree.foldLeft("")((acc, x) => s"$x $acc")
        s1 shouldBe "my dog likes summer "

    describe("Option"):
      it("should fold a None to the zero value"):
        (None: Option[Int]).foldLeft(0)((_, _) => ???) shouldBe 0
        (None: Option[Int]).foldRight(0)((_, _) => ???) shouldBe 0

      it("should fold a Some according to the given function"):
        Option(1).foldLeft(1)((acc, x) => acc * x * 2) shouldBe 2
        Option(1).foldLeft(1)((x, acc) => acc * x * 2) shouldBe 2
