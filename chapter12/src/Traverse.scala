import ApplicativeInstances.{Const, monoidApplicative}
import MonadInstances.{Id, stateMonad, idMonad}

trait Traverse[F[_]] extends Functor[F], Foldable[F]:
  extension [A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
      fa.map(f).sequence

    /*
    Exercise 12.14: Implement map in terms of traverse.
     */
    def map[B](f: A => B): F[B] =
      fa.traverse[Id, B](f)(using idMonad)

    override def foldMap[B: Monoid](f: A => B): B =
      fa.traverse[Const[B, _], Nothing](f)

    def mapAccum[S, B](s: S)(f: (A, S) => (B, S)): (F[B], S) =
      fa.traverse(a =>
        for
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _ <- State.set(s2)
        yield b
      ).run(s)

    override def toList: List[A] =
      fa.mapAccum(List[A]())((a, s) => ((), a :: s))(1).reverse

    def zipWithIndex: F[(A, Int)] =
      fa.mapAccum(0)((a, s) => ((a, s), s + 1))(0)

    /*
    Exercise 12.16: Write a function to reverse any traversable functor.
     */
    def reverse: F[A] =
      fa.mapAccum(fa.toList.reverse)((_, as) => (as.head, as.tail))(0)

    /*
    Exercise 12.17: Use mapAccum to give a default implementation of foldLeft.
     */
    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      fa.mapAccum(acc)((a, b) => ((), f(b, a)))(1)

  extension [G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] =
      fga.traverse(ga => ga)

  /*
  Exercise 12.19: Implement the composition of two Traverse instances.
   */
  def compose[G[_]: Traverse]: Traverse[[x] =>> F[G[x]]] =
    val self = this
    new:
      extension [A](fga: F[G[A]])
        override def traverse[H[_]: Applicative, B](f: A => H[B]): H[F[G[B]]] =
          self.traverse(fga)(ga => ga.traverse(f))

/*
Exercise 12.13: Write Traverse instances for List, Option, Tree, and [x] =>> Map[K, x].
 */
object TraverseInstances:

  given listTraverse: Traverse[List] with
    extension [A](as: List[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
        val g = summon[Applicative[G]]
        as.foldRight(g.unit(List[B]()))((a, acc) => f(a).map2(acc)(_ :: _))

    given optionTraverse: Traverse[Option] with
      extension [A](oa: Option[A])
        override def traverse[G[_]: Applicative, B](
            f: A => G[B]
        ): G[Option[B]] =
          oa match
            case Some(a) => f(a).map(Some(_))
            case None    => summon[Applicative[G]].unit(None)

    case class Tree[+A](head: A, tail: List[Tree[A]])
    /*
    We immediately invoke f with the head value, which gives us a G[B].
    We then traverse the tail list of subtree and recursively traverse
    each of them, which gives us a G[List[B]]. We use map2 to combine
    the head G[B] and subtree G[List[B]] into a single G[Tree[B]]
     */
    given treeTraverse: Traverse[Tree] = new:
      extension [A](ta: Tree[A])
        override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
          f(ta.head).map2(ta.tail.traverse(a => a.traverse(f)))(Tree(_, _))

    given mapTraverse[K]: Traverse[Map[K, _]] with
      extension [A](m: Map[K, A])
        override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Map[K, B]] =
          m.foldLeft(summon[Applicative[G]].unit(Map.empty[K, B])):
            case (acc, (k, a)) =>
              acc.map2(f(a))((m, b) => m + (k -> b))
