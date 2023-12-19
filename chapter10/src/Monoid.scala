trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:
  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(a: A, b: A) = m.combine(b, a)
    val empty               = m.empty

  /*
  Exercise 10.5: Implement foldMap.
   */
  def foldMap[A, B](as: List[A])(f: A => B)(using m: Monoid[B]): B =
    as.foldLeft(m.empty)((acc, x) => m.combine(acc, f(x)))

  /*
  Exercise 10.6: Write foldLeft and foldRight using foldMap.
   */
  def foldLeft[A, B: Monoid](as: List[A])(acc: B)(f: (B, A) => B): B =
    import MonoidInstances.endoMonoid
    foldMap(as)(a => b => f(b, a))(acc)

  def foldRight[A, B: Monoid](as: List[A])(acc: B)(f: (A, B) => B): B =
    import MonoidInstances.endoMonoid
    /*
    curried(f) = A => B => B

    We need to flip the order of arguments to the endoMonoid
    because in the foldMap implementation using foldLeft,
    acc: B is 1st argument, which is passed to curried(f)
    as the 2nd argument.

    Example:
      Without dual, we end up with the following:
        Monoid.foldRight(List("a", "b", "c"))("")(_ + _)
        ==> "cba"
     */
    foldMap(as)(f.curried)(using dual(endoMonoid[B]))(acc)

  /*
  Exercise 10.7: Implement a foldMap for IndexedSeq. Your implementation should use
  the strategy of splitting the sequence in two, recursively processing each half,
  and then adding the answers together with the monoid.
   */
  def foldMapV[A, B](as: IndexedSeq[A])(f: A => B)(using m: Monoid[B]): B =
    as.length match
      case 0 => m.empty
      case 1 => f(as(0))
      case _ =>
        val mid           = as.length / 2
        val (left, right) = as.splitAt(mid)
        m.combine(foldMapV(left)(f), foldMapV(right)(f))

object MonoidInstances:
  /*
  Exercise 10.1: Give Monoid instances for integer addition
  and multiplication as well as the Boolean operators.
   */
  given intAddition: Monoid[Int] with
    def combine(a1: Int, a2: Int) = a1 + a2
    val empty                     = 0

  val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1 * a2
    val empty                     = 1

  given string: Monoid[String] with
    def combine(a1: String, a2: String) = a1 + a2
    val empty                           = ""

  val booleanOr: Monoid[Boolean] = new:
    def combine(x: Boolean, y: Boolean) = x || y
    val empty                           = false

  val booleanAnd: Monoid[Boolean] = new:
    def combine(x: Boolean, y: Boolean) = x && y
    val empty                           = true

  /*
  Exercise 10.2: Given a Monoid instance for combining Option values.
   */
  def optionMonoid[A]: Monoid[Option[A]] = new:
    // Since the book doesn't introduce Semigroup,
    // we don't know how to combine two Some[A] instances.
    def combine(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match
      case (None, None) => None
      case (a, None)    => a
      case (_, b)       => b

    val empty: Option[A] = None

  /*
  Exercise 10.3: A function having the same argument
  and return type is sometimes called an endofunction.
  Write a monoid for endofunctions.
   */
  given endoMonoid[A]: Monoid[A => A] with
    def combine(f: A => A, g: A => A): A => A = f andThen g

    val empty: A => A = identity

  given stringMonoid: Monoid[String] with
    def combine(a1: String, a2: String): String = a1 ++ a2
    val empty: String                           = ""

  /*
  Exercise 10.10: Write a monoid instance for WC.
   */
  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  val wcMonoid = new Monoid[WC]:
    def combine(wc1: WC, wc2: WC): WC = (wc1, wc2) match
      case (WC.Stub(a), WC.Stub(b))       => WC.Stub(a + b)
      case (WC.Stub(a), WC.Part(l, w, r)) => WC.Part(a + l, w, r)
      case (WC.Part(l, w, r), WC.Stub(a)) => WC.Part(l, w, r + a)
      case (WC.Part(l, w, r), WC.Part(l2, w2, r2)) =>
        WC.Part(l, w + (if (r + l2).isEmpty then 0 else 1) + w2, r2)
    def empty: WC = WC.Stub("")

  /*
  Exercise 10.16: Implement productMonoid using a ma: Monoid[A] and mb: Monoid[B].
  Notice that your implementation of combine is associative so long as ma.combine
  and mb.combine are both associative.
   */
  import MonoidSyntax.*
  given productMonoid[A, B](using
      ma: Monoid[A],
      mb: Monoid[B]
  ): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = (x._1 |+| y._1, x._2 |+| y._2)
    val empty                         = (ma.empty, mb.empty)

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(empty): (acc, k) =>
        acc.updated(k, a.getOrElse(k, mv.empty) |+| b.getOrElse(k, mv.empty))
    val empty = Map()

  /*
  Exercise 10.17: Write a monoid instance for functions whose results are monoids.
   */
  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B): A => B =
      (a: A) => f(a) |+| g(a)
    val empty: A => B = _ => mb.empty

object MonoidSyntax:
  extension [T](a: T)(using m: Monoid[T]) def |+|(b: T): T = m.combine(a, b)
