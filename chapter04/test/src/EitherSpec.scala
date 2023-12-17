import Either.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class EitherSpec extends AnyFunSpec:
  describe("Either"):
    it("map should transform a Right"):
      Right(2).map(_ + 2) shouldBe Right(4)

    it("map should have no effect on a Left"):
      (Left(2): Either[Int, Int]).map(_ + 2) shouldBe Left(2)

    it("flatMap should transform a Right"):
      Right(2).flatMap(x => Right(x + 2)) shouldBe Right(4)

    it("flatMap should have no effect on a None"):
      (Left(2): Either[Int, Int]).flatMap(x => Right(x + 2)) shouldBe Left(2)

    it("orElse should return the first Either if it's a Right"):
      Right(2).orElse(???) shouldBe Right(2)

    it("orElse should return the second Either when invoked on a Left"):
      (Left(2): Either[Int, Int]).orElse { Right(3) } shouldBe Right(3)
      (Left(2): Either[Int, Int]).orElse { Left(3) } shouldBe Left(3)

    it("map2 should combine two Either values"):
      Right(2).map2(Right(3))(_ * _) shouldBe Right(6)

    it("map2 should return Left if any one of the arguments is a Left"):
      val l: Either[Int, Int] = Left(3)
      val r: Either[Int, Int] = Right(2)

      r.map2(l)(_ * _) shouldBe Left(3)
      l.map2(r)(_ * _) shouldBe Left(3)
      l.map2(l)(_ * _) shouldBe Left(3)

    it("sequence should collect all the Right values"):
      sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))

    it("sequence should stop at first Left"):
      sequence(List(Right(1), Left(2), Right(3))) shouldBe Left(2)
