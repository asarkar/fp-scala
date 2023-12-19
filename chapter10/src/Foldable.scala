trait Foldable[F[_]]:
  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B
    def foldLeft[B](acc: B)(f: (B, A) => B): B
    def foldMap[B](f: A => B)(using m: Monoid[B]): B =
      foldLeft(m.empty)((acc, x) => m.combine(acc, f(x)))
    def combineAll(using m: Monoid[A]): A =
      as.foldLeft(m.empty)(m.combine)
    /*
    Exercise 10.15: Any Foldable structure can be turned into a List.
    Add a toList extension method to the Foldable trait, and provide
    a concrete implementation in terms of the other methods on Foldable.
     */
    def toList: List[A] = foldRight(List.empty[A])(_ :: _)

object FoldableInstances:
  /*
  Exercise 10.12: Implement Foldable[List], Foldable[IndexedSeq], and Foldable[LazyList].
  Remember that foldRight, foldLeft, and foldMap can all be implemented in terms of each other,
  but that might not be the most efficient implementation.
   */
  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def toList: List[A] = as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        Monoid.foldMapV(as)(f)(using mb)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)

  /*
  Exercise 10.13: Implement a Foldable instance for a Tree.
   */
  enum Tree[+A]:
    case Leaf(value: A)
    case Branch(left: Tree[A], right: Tree[A])

  given Foldable[Tree] with
    extension [A](t: Tree[A])
      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        foldRight(acc)((a, b) => f(b, a))

      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        t match
          case Tree.Leaf(x)      => f(x, acc)
          case Tree.Branch(l, r) => l.foldRight(r.foldRight(acc)(f))(f)

  /*
  Exercise 10.14: Write a Foldable[Option] instance.
   */
  given Foldable[Option] with
    extension [A](op: Option[A])
      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        op match
          case Some(x) => f(acc, x)
          case _       => acc
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        foldLeft(acc)((b, a) => f(a, b))
