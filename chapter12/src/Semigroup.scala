trait Semigroup[A]:
  def combine(a1: A, a2: A): A

trait Monoid[A] extends Semigroup[A]:
  def empty: A

enum Validated[+E, +A]:
  case Valid(get: A)
  case Invalid(error: E)

case class NonEmptyList[+A](head: A, tail: List[A]):
  def toList: List[A] = head :: tail

object NonEmptyList:
  def apply[A](head: A, tail: A*): NonEmptyList[A] =
    NonEmptyList(head, tail.toList)

object SemigroupInstances:
  given nelSemigroup[A]: Semigroup[NonEmptyList[A]] with
    def combine(x: NonEmptyList[A], y: NonEmptyList[A]) =
      NonEmptyList(x.head, x.tail ++ (y.head :: y.tail))
