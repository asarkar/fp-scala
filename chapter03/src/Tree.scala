enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int =
    fold(_ => 1, 1 + _ + _)
  /*
    Exercise 3.26: Write a function, depth, that returns the maximum
    path length from the root to any leaf.
   */
  def depth: Int =
    fold(_ => 0, (d1, d2) => 1 + (d1 max d2))

  /*
    Exercise 3.27: Write a function, map, analogous to the method of
    the same name on List that modifies each element in a tree with
    a given function.
   */
  def map[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)), Branch(_, _))

  /*
    Exercise 3.28: Generalize size, maximum, depth, and map, writing
    a new function, fold, that abstracts over their similarities.
   */
  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

object Tree:
  /*
    Exercise 3.25: Write a function, maximum, that returns the
    maximum element in a Tree[Int].
   */
  def maximum(t: Tree[Int]): Int =
    t.fold(x => x, (x, y) => x.max(y))
