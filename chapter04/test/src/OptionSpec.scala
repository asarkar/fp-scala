import Option.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class OptionSpec extends AnyFunSpec:
  describe("Option"):
    it("map should transform a Some"):
      Some(2).map(_ + 2) shouldBe Some(4)

    it("map should have no effect on a None"):
      Some(2).filter(_ % 2 == 1).map(_ + 2) shouldBe None

    it("getOrElse should return the value inside a Some"):
      Some(2).getOrElse(???) shouldBe 2

    it("getOrElse should return the default value when invoked on a None"):
      Some(2).filter(_ % 2 == 1).getOrElse { 3 } shouldBe 3

    it("filter should keep the value if it satisfies the given predicate"):
      Some(2).filter(_ % 2 == 0) shouldBe Some(2)

    it("filter should convert Some to None if the value doesn't satisfy the given predicate"):
      Some(2).filter(_ % 2 == 1) shouldBe None

    it("flatMap should transform a Some"):
      Some(2).flatMap(x => Some(x + 2)) shouldBe Some(4)

    it("flatMap should have no effect on a None"):
      Some(2).filter(_ % 2 == 1).flatMap(x => Some(x + 2)) shouldBe None

    it("orElse should return the first Option if it's a Some"):
      Some(2).orElse(???) shouldBe Some(2)

    it("orElse should return the second Option when invoked on a None"):
      Some(2).filter(_ % 2 == 1).orElse { Some(3) } shouldBe Some(3)

    it("map2 should combine two Some values"):
      map2(Some(2), Some(3))(_ * _) shouldBe Some(6)

    it("map2 should return None if any one of the arguments is a None"):
      map2(Some(2), None: Option[Int])(_ * _) shouldBe None
      map2(None: Option[Int], Some(2))(_ * _) shouldBe None
      map2(None: Option[Int], None: Option[Int])(_ * _) shouldBe None

    it("sequence should collect all the Some values"):
      sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))

    it("sequence should stop at first None"):
      sequence(List(Some(1), None, Some(2))) shouldBe None
