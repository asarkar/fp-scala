trait RNG:
  def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n       = (newSeed >>> 16).toInt
    (n, nextRNG)

object Lib:
  /*
  Exercise 6.1: Write a function that uses RNG.nextInt to generate
  a random integer between 0 and Int.MaxValue (inclusive).
  Make sure to handle the corner case when nextInt returns Int.MinValue
  which doesn't have a nonnegative counterpart.
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  /*
  Exercise 6.2: Write a function to generate a Double
  between 0 and 1, not including 1.
   */
  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)

  /*
  Exercise 6.3: Write functions to generate an (Int, Double) pair,
  a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
  You should be able to reuse the functions you've already written.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (n, nextRNG)     = nonNegativeInt(rng)
    val (d, nextNextRNG) = double(nextRNG)
    (n, d) -> nextNextRNG

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (x, nextRng) = intDouble(rng)
    (x.swap, nextRng)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val x = double(rng)
    val y = double(x._2)
    val z = double(y._2)
    (x._1, y._1, z._1) -> z._2

  /*
  Exercise 6.4: Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    val xs = (1 to count).scanLeft((-1, rng))((x, _) => nonNegativeInt(x._2))
    xs.tail.map(_._1).toList -> xs.last._2
