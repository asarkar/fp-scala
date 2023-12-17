import scala.annotation.tailrec
enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def sum(ints: List[Int]): Int = ints match
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)

  def head[A](xs: List[A]): A = xs match
    case Nil        => sys.error("empty list")
    case Cons(x, _) => x

  /*
  Exercise 3.2: Implement the function tail for removing the first element of a List.
   */
  def tail[A](xs: List[A]): List[A] = xs match
    case Nil         => sys.error("empty list")
    case Cons(_, ys) => ys

  /*
  Exercise 3.3: Implement the function setHead for replacing the first element of a list
  with a different value.
   */
  def setHead[A](xs: List[A], a: A): List[A] = xs match
    case Nil         => Cons(a, Nil)
    case Cons(_, ys) => Cons(a, ys)

  /*
  Exercise 3.4: Implement the function drop, which removes the first n elements from a list.
  Dropping n element from an empty list should return the empty list.
   */
  def drop[A](xs: List[A], n: Int): List[A] =
    if n <= 0 then xs
    else
      xs match
        case Cons(_, ys) => drop(ys, n - 1)
        case Nil         => Nil

  /*
  Exercise 3.5: Implement dropWhile, which removes elements from the List prefix as
  long as they match a predicate.
   */
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Cons(hd, tl) if f(hd) => dropWhile(tl, f)
      case _                     => as

  /*
  Exercise 3.6: Implement a function, init, that returns a list containing of all
  but the last element of a list.
   */
  def init[A](xs: List[A]): List[A] = xs match
    case Nil          => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))

//   def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match
//     case Nil         => acc
//     case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  /*
  Exercise 3.7: Can product, implemented using foldRight, immediately halt the
  recursion and return 0.0 if it encounters a 0.0? Why or why not? Consider how
  any short circuiting might work if you call foldRight with a large list.
  ---
  No, foldRight traverses all the way to the end of the list before invoking the function.
   */

  /*
  Exercise 3.8: See what happens when you pass Nil and Cons themselves to foldRight,
  like this: foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _)).
  What do you think this says about the relationship between foldRight and the data
  constructors of List?
  --
  Nothing happens, the original list is returned.
   */

  /*
  Exercise 3.9: Compute the length of a list using foldRight.
   */
  def length[A](xs: List[A]): Int =
    foldRight(xs, 0, (_, acc) => acc + 1)

  /*
  Exercise 3.10: foldRight is not stack safe. Write another general list-recursion function,
  foldLeft, that is tail recursive. Start collapsing from the leftmost start of the list.
   */
  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match
    case Nil         => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  /*
  Exercise 3.11: Write sum, product and a function to compute the length
  of a list using foldLeft.
   */
  def sumViaFoldLeft(xs: List[Int]): Int =
    foldLeft(xs, 0, _ + _)

  def productViaFoldLeft(xs: List[Int]): Int =
    foldLeft(xs, 1, _ * _)

  def lengthViaFoldLeft(xs: List[Int]): Int =
    foldLeft(xs, 0, (acc, _) => acc + 1)

  /*
  Exercise 3.12: Write a function that returns the reverse of a list.
   */
  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A], (acc, x) => Cons(x, acc))

  /*
  Exercise 3.13: Can you write foldRight in terms of foldLeft? How about the other way around?
  ---
  At each iteration, we create a function that remembers the current list
  element and awaits a value of type B before producing a result.
  Upon receiving such a value, we apply f to produce a value of type B,
  which is then fed to the function from the previous step.

  The output from foldLeft is a function which is fed the zero value,
  at which point the function chain is evaluated in reverse.

  Note that this implementation is not stack safe due to the creation
  of an anonymous function at each step.
  For a stack safe implementation, we can reverse the list and then
  apply foldLeft.
   */
  def foldRight[A, B](as: List[A], z: B, f: (A, B) => B): B = foldLeft(
    as,
    (b: B) => b,
    (acc, a) => (b: B) => acc(f(a, b))
  )(z)

  /*
  Exercise 3.14: Implement append in terms of either foldLeft or foldRight.
   */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, Cons(_, _))

  /*
  Exercise 3.15: Write a function that concatenates a list of lists into a
  single list. Its runtime should be linear in the total length of all lists.
   */
  def concat[A](xxs: List[List[A]]): List[A] =
    foldRight(xxs, List[A](), append)

  /*
  Exercise 3.16: Write a function that transforms a list of integers by adding
  1 to each element.
   */
  def add1(xs: List[Int]): List[Int] =
    foldRight(xs, List[Int](), (x, acc) => Cons(x + 1, acc))

  /*
  Exercise 3.17: Write a function that turns each value in a List[Double] into
  a String.
   */
  def doubleToString(xs: List[Double]): List[String] =
    foldRight(xs, List[String](), (x, acc) => Cons(f"$x%2.2f", acc))

  /*
  Exercise 3.18: Write a function, map, that generalizes modifying each element
  in a list while maintaining the structure of the list.
   */
  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, List[B](), (x, acc) => Cons(f(x), acc))

  /*
  Exercise 3.19: Write a function, filter, that removes elements from a list
  unless they satisfy a given predicate.
   */
//   def filter[A](as: List[A], f: A => Boolean): List[A] =
//     foldRight(as, List[A](), (x, acc) => if f(x) then Cons(x, acc) else acc)

  /*
  Exercise 3.20: Write a function, flatMap, that works like map except that
  the function given will return a list instead of a single result, ensuring
  that the list is inserted into the final resulting list.
   */
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, List[B](), (x, acc) => append(f(x), acc))

  /*
  Exercise 3.21: Use flatMap to implement filter.
   */
  def filter[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, x => if f(x) then List(x) else List())

  /*
  Exercise 3.22: Write a function that accepts two lists and constructs a new
  list by adding corresponding elements.

  Exercise 3.23: Generalize the function you just wrote so it's not specific
  to integers or addition.
   */
  // Not stack safe!
  def zipWith[A, B, C](xs: List[A], ys: List[B], f: (A, B) => C): List[C] = (xs, ys) match
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(x, xxs), Cons(y, yys)) => Cons(f(x, y), zipWith(xxs, yys, f))

  /*
  Exercise 3.24: Implement hasSubsequence to check whether a List contains
  another List as a sunsequence.
   */
  // 1. Does subsequence mean potentially not consecutive elements? No.
  // 2. Check at every position.
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
