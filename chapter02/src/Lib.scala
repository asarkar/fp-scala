import scala.annotation.tailrec

object Lib:
  /*
  Exercise 2.1: Write a recursive function to get the nth Fibonacci number.
  The first two Fibonacci numbers are 0 and 1. The nth number is always the
  sum of the previous two-the sequence begins 0, 1, 1, 2, 3, 5. Your definition
  should use a local, tail-recursive funtion.
   */
  def fib(n: Int): Int =
    @tailrec
    def go(n: Int, current: Int, next: Int): Int =
      if n <= 0 then current
      else go(n - 1, next, current + next)

    go(n, 0, 1)

  /*
  Exercise 2.2: Implement isSorted, which checks whether an Array[A]
  is sorted according to a given comparison function, gt, which returns
  true is the first parameter is greater than the second parameter.
   */
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    as.zipWithIndex.forall((a, i) => i == 0 || gt(a, as(i - 1)))

  /*
  Exercise 2.3: Implement the following function.
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  /*
  Exercise 2.4: Implement uncurry, which reverse the transformation of of curry.
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  /*
  Exercise 2.5: Implement the higher-order function that composes two functions.
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
