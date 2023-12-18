/*
Exercise 6.11: Implement a finite state automaton that models a simple candy dispenser.
The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
It can be in one of two states: locked or unlocked.
It also tracks how many candies are left and how many coins it contains.

The rules of the machine are as follows:

  - Inserting a coin into a locked machine will cause it to unlock if there's any candy left.
  - Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
  - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
  -  A machine that's out of candy ignores all inputs.

The simulateMachine method should operate the machine based on the list of inputs and
return the number of coins and candies left in the machine at the end.

For example, if the input Machine has 10 coins and five candies, and a total of four candies
are successfully bought, the output should be (14, 1).
 */

object CandyDispenser:
  enum Input:
    case Coin, Turn

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      // The return type is a List[Unit], since State.modify discards the value.
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)

  val update = (i: Input) =>
    (s: Machine) =>
      (i, s) match
        case (_, Machine(_, 0, _))              => s
        case (Input.Coin, Machine(false, _, _)) => s
        case (Input.Turn, Machine(true, _, _))  => s
        case (Input.Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Input.Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
