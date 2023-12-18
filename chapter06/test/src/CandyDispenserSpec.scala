import CandyDispenser.*

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class CandyDispenserSpec extends AnyFunSpec:
  describe("CandyDispenser"):
    it("should dispense candies"):
      val machine = Machine(true, 5, 10)
      val inputs: List[Input] = List
        .fill(4)(Input.Coin)
        .zip(List.fill(4)(Input.Turn))
        .flatMap(x => List(x._1, x._2))

      val (a, _) = simulateMachine(inputs).run(machine)
      a shouldBe (14, 1)
