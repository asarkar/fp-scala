trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    /*
    Exercise 11.8: Implement flatMap in terms of compose.
     */
    def flatMap[B](f: A => F[B]): F[B]
    // compose returns Any => F[B].
    // Commenting out to prevent cyclic defintion.
    // compose(const(fa), f)(const)

    /*
      Exercise 11.13: Implement either flatMap or compose in terms of join and map.
     */
    // join(fa.map(f))

    def map[B](f: A => B): F[B] =
      flatMap(f.andThen(unit))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

  def const[A](fa: F[A])(x: Any): F[A] = fa
  /*
  Exercise 11.3: Implement sequence and traverse on Monad[F].
   */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, b) => f(a).map2(b)(_ :: _))

  /*
  Exercise 11.4: Implement replicateM.
   */
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  /*
  Exercise 11.5: Implement the function filterM-it's a bit like filter,
  except instead of a function from A => Boolean, we have an A => F[Boolean].
   */
  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    def takeOrDrop(a: A)(keep: Boolean, acc: List[A]): List[A] =
      if keep then a :: acc else acc

    val z = unit(List.empty[A])
    as.foldRight(z)((a, b) => f(a).map2(b)(takeOrDrop(a)))

  /*
  Exercise 11.7: Implement the Kleisli composition function compose.
   */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    f.andThen(_.flatMap(g))

  /*
  Exercise 11.12: Implement join in terms of flatMap.
   */
  def join[A](ffa: F[F[A]]): F[A] =
    ffa.flatMap(identity)

object MonadInstances:
  /*
  Exercise 11.1: Write monad instances for Option, List, LazyList, Par, and Parser.
   */
  given optionMonad: Monad[Option] with
    def unit[A](a: => A): Option[A] = Some(a)
    extension [A](fa: Option[A])
      def flatMap[B](f: A => Option[B]): Option[B] =
        fa match
          case Some(a) => f(a)
          case _       => None

  given listMonad: Monad[List] with
    def unit[A](a: => A): List[A] = List(a)
    extension [A](fa: List[A])
      def flatMap[B](f: A => List[B]): List[B] =
        fa match
          case Nil     => Nil
          case x :: xs => f(x) ++ xs.flatMap(f)

  /*
  Exercise 11.2: Implement a State monad.
   */
  // _ in type constructor requires kind-projector:underscores plugin enabled.
  // Alternatively, Monad[[x] =>> State[S, x]]
  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)

  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A]                                                = Right(a)
    extension [A](e: Either[E, A]) def flatMap[B](f: A => Either[E, B]): Either[E, B] = e.flatMap(f)

object MonadSyntax:
  // 'fA' is the thing on which the method >>= is invoked.
  extension [F[_], A](fA: F[A])(using mA: Monad[F]) infix def >>=[B](f: A => F[B]): F[B] = mA.flatMap(fA)(f)
