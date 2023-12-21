trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

trait Applicative[F[_]] extends Functor[F]:
  // primitive combinators
  def unit[A](a: => A): F[A]
  /*
  Exercise 12.2: Define in terms of map2.
   */
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    fab.map2(fa)((f, a) => f(a))

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
    p match
      case (a, (b, c)) => ((a, b), c)

  /*
  Exercise 12.8: We can take the product of two applicative functors.
  Implement this function on the Applicative trait.
   */
  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] =
    val self = this
    new:
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs(0))(p(0)), G.apply(fs(1))(p(1)))

  /*
  Exercise 12.9: Applicative functors compose! If F[_] and G[_] are applicative functors,
  then so is F[G[_]]. Implement this function on the Applicative trait.
   */
  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] =
    val self = this
    new:
      def unit[A](a: => A) = self.unit(G.unit(a))
      extension [A](fga: F[G[A]])
        override def map2[B, C](fgb: F[G[B]])(f: (A, B) => C) =
          self.map2(fga)(fgb)(G.map2(_)(_)(f))

  // derived combinators
  extension [A](fa: F[A])
    /*
    Exercise 12.2: Define in terms of apply and unit.
     */
    def map[B](f: A => B): F[B] =
      //   fa.map2(unit(()))((a, _) => f(a))
      apply(unit(f))(fa)

    /*
    Exercise 12.2: Define in terms of apply and map.
     */
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

  /*
  Exercise 12.1: Implement sequence, traverse, replicateM, and product
  using only map2 and unit or methods implemented in terms of them.
   */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, b) => f(a).map2(b)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  /*
  Exercise 12.12: Implement sequence over a Map.
   */
  def sequenceMap[K, V](ofv: Map[K, F[V]]): F[Map[K, V]] =
    ofv.foldLeft(unit(Map.empty[K, V])):
      case (acc, (k, fv)) =>
        acc.map2(fv)((m, v) => m + (k -> v))

object ApplicativeInstances:

  /*
  Exercise 12.6: Write an Applicative instance for Validated that accumulates errors in Invalid.
   */
  import Validated.*

  given validatedApplicative[E: Semigroup]: Applicative[Validated[E, _]] with
    def unit[A](a: => A) = Valid(a)

    extension [A](fa: Validated[E, A])
      override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C) =
        (fa, fb) match
          case (Valid(a), Valid(b)) => Valid(f(a, b))
          case (Invalid(e1), Invalid(e2)) =>
            Invalid(summon[Semigroup[E]].combine(e1, e2))
          case (Invalid(e), _) => Invalid(e)
          case (_, Invalid(e)) => Invalid(e)

  given OptionApplicative: Applicative[Option] with
    def unit[A](a: => A) = Some(a)

    extension [A](fa: Option[A])
      override def map2[B, C](fb: Option[B])(f: (A, B) => C) =
        (fa, fb) match
          case (Some(a), Some(b)) => Some(f(a, b))
          case (None, _)          => None
          case (_, None)          => None

  type Const[A, B] = A

  given monoidApplicative[M](using
      m: Monoid[M]
  ): Applicative[Const[M, _]] with
    def unit[A](a: => A): M = m.empty
    override def apply[A, B](m1: M)(m2: M): M =
      m.combine(m1, m2)
