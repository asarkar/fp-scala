// https://docs.scala-lang.org/scala3/book/types-opaque-types.html
// Like Haskell newtype.
opaque type State[S, +A] = S => (A, S)

object State:
  // Multiple extension methods defined on type State.
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    /*
    Exercise 6.10: Generalize the functions unit, map, map2,
    flatMap, and sequence.
     */
    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    (s: S) => (a, s)

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    // The value 'a' in unit is a List.empty[B]. We run each action,
    // then combine it with the current state 'acc'.
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))
