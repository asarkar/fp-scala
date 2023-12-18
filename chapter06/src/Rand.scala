import Lib.*

type Rand[+A] = RNG => (A, RNG)

object Rand:
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](r: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, r1) = r(rng)
      (f(a), r1)

  /*
  Exercise 6.5: Use map to reimplement double in a more succinct way.
   */
  val double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /*
  Exercise 6.6: Implement the function map2, that takes two actions,
  ra and rb, and a function f, for combining their results and returns
  a new action that combines them.
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)

  /*
  Exercise 6.7: Implement sequence for combining a List of actions into
  a single action. Use it to reimplement the ints function you wrote
  before.
   */
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  def intsViaSequence(count: Int)(rand: Rand[Int]): Rand[List[Int]] =
    sequence(List.fill(count)(rand))

  /*
  Exercise 6.8: Implement flatMap.
   */
  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, r1) = r(rng)
      // f: A => RNG => (B, RNG)
      f(a)(r1)

  /*
  Exercise 6.9: Reimplement map and map2 in terms of flatMap.
   */
  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
