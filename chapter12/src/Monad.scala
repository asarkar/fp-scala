trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

    override def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    override def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  extension [A](ffa: F[F[A]]) def join: F[A] = ffa.flatMap(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

object MonadInstances:
  /*
  Exercise 12.5: Write a Monad instance for Either.
   */
  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A] = Right(a)
    extension [A](eea: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]) = eea match
        case Right(a) => f(a)
        case Left(b)  => Left(b)

  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)

  type Id[A] = A
  given idMonad: Monad[Id] with
    def unit[A](a: => A)                                      = a
    extension [A](a: A) override def flatMap[B](f: A => B): B = f(a)
