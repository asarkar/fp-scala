enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  /*
  Exercise 5.1: Write a function to convert a LazyList to a List,
  which will force its evaluation.
   */
  def toList: List[A] =
    // foldRight(() => List.empty[A])((a, b) => () => a :: b())()
    this match
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList

  // stack-safe version
  // def toList: List[A] =
  //   @tailrec
  //   def go(ll: LazyList[A], acc: List[A]): List[A] =
  //     ll match
  //       case Cons(h, t) => go(t(), h() :: acc)
  //       case Empty => acc.reverse
  //   go(this, Nil)

  /*
  Exercise 5.2: Write a function take(n) for returning the first n elements
  of a LazyList and drop(n) for skipping the first n elements of a LazyList.
   */
  def take(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) =>
      if n == 0 then Empty
      else Cons(h, () => t().take(n - 1))

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  /*
  Exercise 5.3: Write the function takeWhile for returning all
  starting elements of a LazyList that match the given predicate.

  Exercise 5.5: Use foldRight to implement takeWhile.
   */
  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else empty)

  // The arrow => in front of the argument type B means the function f
  // takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      // If f doesn't evaluate its second argument, the recursion never occurs.
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _          => acc

  /*
  Exercise 5.4: Implement forAll, which checks that all elements in a LazyList
  match a given predicate. Your implementation should terminate the traversal
  as soon as it encounters a nonmatching value.
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /*
  Exercise 5.6: Implement headOption using foldRight.
   */
  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  /*
  Exercise 5.7: Implement map, filter, append, and flatMap using foldRight.
  The append method should be nonstrict in its argument.
   */
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else b)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty[B])(f(_).append(_))

  def tail: LazyList[A] = drop(1)

  /*
  Exercise 5.13: Use unfold to implement map, take, takeWhile,
  zipWith, and zipAll. The zipAll function should continue the
  traversal as long as either lazy list has more elements; it
  uses Option to indicate whether each lazy list has been exhausted.
   */
  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this)(s => s.headOption.map(a => (f(a), s.tail)))

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n))((s, i) => s.headOption.filter(_ => i > 0).map(a => (a, (s.tail, i - 1))))

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this)(s => s.headOption.filter(p).map(a => (a, s.tail)))

  def zipWith[B, C](that: LazyList[B], f: (A, B) => C): LazyList[C] =
    unfold((this, that)):
      case (Empty, _)                   => None
      case (_, Empty)                   => None
      case (Cons(x, xxs), Cons(y, yys)) => Some(f(x(), y()) -> (xxs() -> yys()))

  def zipAll[B, C](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Empty, Empty)               => None
      case (Empty, Cons(y, yys))        => Some((None -> Some(y()), Empty -> yys()))
      case (Cons(x, xxs), Empty)        => Some((Some(x()) -> None, xxs() -> Empty))
      case (Cons(x, xxs), Cons(y, yys)) => Some((Some(x()) -> Some(y()), xxs() -> yys()))

  /*
  Exercise 5.14: Implement startsWith using functions you've written.
  It should check if one LazyList is a prefix of another.
   */
  // What is the expectation for prefix = Empty?
  def startsWith[A](prefix: LazyList[A]): Boolean =
    zipAll(prefix)
      .map:
        case (Some(x), Some(y)) => x == y
        case (_, None)          => true
        case _                  => false
      .filter(x => !x)
      .headOption
      .getOrElse(true)

  /*
  Exercise 5.15: Implement tails using unfold. For a given LazyList, tails
  returns the LazyList of suffixes of the input sequence, starting with the
  original LazyList.
   */
  def tails: LazyList[LazyList[A]] =
    unfold(Option(this)):
      case Some(Empty) => Some(Empty, None)
      case Some(xs)    => Option(xs, Option(xs.tail))
      case _           => None

  /*
  Exercise 5.16: Generalize tails to the function scanRight, which is like a foldRight
  that returns a lazy list of the intermediate results.
  ---
  We can’t implement scanRight via unfold because unfold builds a lazy list from left to right.
  Instead, we can use foldRight with a slight modification. We build a lazy list from right to left
  where the head of the list is the accumulated value for the list suffix after the current node.

    Example:
      LazyList(1, 2, 3).scanRight(0)(_ + _)
        ==> (a=3, b=LazyList(0))
        ==> (a=2, b=LazyList(3, 0))
        ==> (a=1, b=LazyList(5, 3, 0))
        => LazyList(6, 5, 3, 0)
   */
  def scanRight[B](z: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(LazyList(z)): (a, b) =>
      val b1 = f(a, b.headOption.get)
      cons(b1, b)

object LazyList:
  def cons[A](
      hd: => A,
      tl: => LazyList[A]
  ): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  /*
  Exercise 5.8: Implement continually, which returns an infinite
  LazyList of a given value.
   */
  def continually[A](a: A): LazyList[A] =
    cons(a, continually(a))

  /*
  Exercise 5.9: Write a function that generates an infinite lazy list
  of integers starting from n, then n + 1, n + 2, and so on.
   */
  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  /*
  Exercise 5.10: Write a function fibs that generates the infinite lazy
  list of Fibonacci numbers.
   */
  val fibs: LazyList[Int] =
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)

  /*
  Exercise 5.11: Write a more general LazyList-building function
  called unfold. It takes an initial state and a function for
  producing both the next state and the next value in the generated
  lazy list.
   */
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some(x, xs) => cons(x, unfold(xs)(f))
      case _           => Empty

  /*
  Exercise 5.12: Write fibs, continually, and ones in terms of unfold.
   */
  val onesViaUnfold: LazyList[Int] =
    unfold(0)(s => Some(1, s))

  val fibsViaUnfold: LazyList[Int] =
    unfold(List(0, 1))(s => Some(s.head, List(s(1), s.sum)))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(s => Some(s, s + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(s => Some(s, s))
