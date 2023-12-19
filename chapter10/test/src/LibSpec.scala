import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import scala.util.Random
import Lib.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

class LibSpec extends AnyFunSpec with ScalaCheckPropertyChecks:
  describe("Monoid"):
    it("isOrdered should be able to detect if a given IndexedSeq[Int] is ordered"):
      def genIdxSeqN(n: Int) = Gen.containerOfN[IndexedSeq, Int](n, Gen.choose(0, n))
      val genIdxSeq = Gen
        .choose(0, 10)
        .flatMap(genIdxSeqN)

      forAll(genIdxSeq) { (xs: IndexedSeq[Int]) =>
        val sortedXs = xs.sorted
        isOrdered(sortedXs) shouldBe true

        // Prevent infinite loop for lists with all elements equal.
        if xs.toSet.size > 1 then
          val as = Iterator
            .unfold(xs) { ys =>
              val zs = Random.shuffle(ys)
              Some((zs, zs))
            }
            .dropWhile(_ == sortedXs)
            .take(1)
            .next()

          isOrdered(as) shouldBe false
      }

    it("bag should calculate the count of each element in it"):
      bag(IndexedSeq("a", "rose", "is", "a", "rose")) shouldBe Map("a" -> 2, "rose" -> 2, "is" -> 1)
