import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Gen
import org.scalatest.AppendedClues.convertToClueful

class MonadSpec extends AnyFunSpec with ScalaCheckDrivenPropertyChecks:
  val genInt       = Gen.choose(-100, 100)
  val genOptionInt = Gen.option(genInt)
  val genEitherint = Gen.either(genInt, genInt)
  def genList[A](g: Gen[A]) = Gen
    .choose(0, 100)
    .flatMap(Gen.listOfN(_, g))

  describe("Monad"):
    it("sequence should return None if there's a None in the input list"):
      forAll(genList(genOptionInt)) { xs =>
        MonadInstances.optionMonad
          .sequence(xs) match
          case Some(ys) => ys shouldBe xs.map(_.get)
          case _        => xs.exists(_.isEmpty) shouldBe true
      }

    it("sequence should return first Left in the input list"):
      forAll(genList(genEitherint)) { xs =>
        MonadInstances
          .eitherMonad[Int]
          .sequence(xs) match
          case Left(x)   => xs.find(_.isLeft).get
          case Right(ys) => ys shouldBe xs.flatMap(_.toSeq)
      }

    it("traverse should replace the None values in the input list"):
      forAll(genList(genOptionInt)) { xs =>
        MonadInstances.optionMonad
          .traverse(xs)(_.orElse(Option(1))) match
          case Some(ys) => ys shouldBe xs.map(_.getOrElse(1))
          case _        => fail("shouldn't be here")
      }

    it("traverse should return the Left values in the input list"):
      forAll(genList(genEitherint)) { xs =>
        MonadInstances
          .eitherMonad[Int]
          .traverse(xs)(_.orElse(Right(1))) match
          case Right(ys) => ys shouldBe xs.map(_.getOrElse(1))
          case _         => fail("shouldn't be here")
      }

    it("replicateM should return None if the input is None or else n copies of the value"):
      forAll(genOptionInt.flatMap(x => Gen.choose(1, 10).map(n => (n, x)))) { (n, x) =>
        MonadInstances.optionMonad
          .replicateM(n, x) match
          case Some(xs) => xs shouldBe List.fill(n)(x.get)
          case _        => x shouldBe None

      }

    it("filterM should return an empty list when looking for an odd number in all evens"):
      forAll(genList(genInt)) { xs =>
        MonadInstances.optionMonad
          .filterM(xs.map(_ * 2))(x => Some(x % 2 == 1)) match
          case Some(ys) => ys.isEmpty `withClue` (s", actual = $ys")
          case _        => fail("shouldn't be here")
      }
