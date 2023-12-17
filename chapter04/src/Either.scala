enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  /*
  Exercise 4.6: Implement versions of map, flatMap, orElse, and map2
  on Either that operate on the Right value.
   */
  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => f(a)
    case Left(e)  => Left(e)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => Right(a)
    case Left(_)  => b

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    // flatMap(x => that.map(y => f(x, y)))
    for
      a <- this
      b <- that
    yield f(a, b)

object Either:
  /*
  Exercise 4.7: Implement sequence and traverse for Either. These should return
  the first error that's encountered if there is one.
   */
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(a => a)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match
      case x :: xs => f(x).flatMap(a => traverse(xs)(f).map(a :: _))
      case _       => Right(Nil)
