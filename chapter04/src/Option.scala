enum Option[+A]:
  case Some(get: A)
  case None

  /*
  Exersie 4.1: Implement the following functions on Option.
  You should be able to implement all the functions besides
  map and getOrElse without resorting to pattern matching.
  Try implementing flatMap, orElse, and filter in terms of
  map and getOrElse.
   */
  def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(x) => Some(f(x))

  def getOrElse[B >: A](default: => B): B = this match
    case None    => default
    case Some(x) => x

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    // map(x => if f(x) then Some(x) else None)
    // .getOrElse(None)
    flatMap(x => if f(x) then this else None)

object Option:
  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  /*
  Exercise 4.2: Implement the variance function in terms of flatMap.
  If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2)
  for each element of the sequence.
   */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /*
  Exercise 4.3: Write a generic function map2 that combines two Option values
  using a binary function. If either Option is None, then the return value is too.
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  /*
  Exercise 4.4: Write a function sequence that combines a list of Options into
  one Option containing a list of all the Some values on the original list.
  If the original list contains None even once, the result of the function
  should be None; otherwise, the result should be Some, with a list of all
  the values.
   */
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)

  /*
  Exercise 4.5: Sometimes we'll want to map over a list using a function
  that might fail, returning None if applying it to any element of the
  list returns None.
  Implement this function. It's straightforward to do using map and sequence,
  but try for a more efficient implementation that only looks at the list once.
  In fact, implement sequence in in terms of traverse.
   */
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match
      case x :: xs => f(x).flatMap(a => traverse(xs)(f).map(a :: _))
      case _       => Some(Nil)
