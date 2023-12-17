import Tree.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class TreeSpec extends AnyFunSpec:
  describe("Tree"):
    val t = Branch(Branch(Leaf(0), Leaf(1)), Branch(Leaf(2), Leaf(3)))

    it("size should return the number of nodes in the tree"):
      t.size shouldBe 7

    it("depth should return the maximum path length from the root to any leaf"):
      t.depth shouldBe 2

    it("map should transform each node but retain the tree structure"):
      t.map('a' + _) shouldBe
        Branch(Branch(Leaf('a'), Leaf('b')), Branch(Leaf('c'), Leaf('d')))

    it("maximum should transform each node but retain the tree structure"):
      maximum(t) shouldBe 3
