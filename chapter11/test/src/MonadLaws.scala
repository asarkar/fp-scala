import org.scalacheck.Arbitrary

import MonadSyntax.*
import org.scalactic.Equality
import scala.language.adhocExtensions
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers.shouldBe

trait MonadLaws[F[_]](desc: String) extends ScalaCheckPropertyChecks with AnyFunSpecLike:
  // Monad[F].unit(x).flatMap(f) === f(x)
  def leftIdentity[A, B](using
      Monad[F],
      Arbitrary[A],
      Arbitrary[A => F[B]],
      Equality[F[B]]
  ): Unit =
    it("should satisfy the left identity law"):
      forAll { (a: A, f: A => F[B]) =>
        val lhs = summon[Monad[F]].unit(a) >>= f
        val rhs = f(a)

        lhs shouldBe rhs
      }

  // m.flatMap(Monad[F].unit) === m
  def rightIdentity[A](using
      Monad[F],
      Arbitrary[F[A]],
      Equality[F[A]]
  ): Unit =
    it("should satisfy the right identity law"):
      forAll { (fa: F[A]) =>
        val lhs = fa >>= summon[Monad[F]].unit
        val rhs = fa

        lhs shouldBe rhs
      }

  // m.flatMap(f).flatMap(g) === m.flatMap { x => f(x).flatMap(g) }
  def associativity[A, B, C](using
      Monad[F],
      Arbitrary[F[A]],
      Arbitrary[A => F[B]],
      Arbitrary[B => F[C]],
      Equality[F[C]]
  ): Unit =
    it("should satisfy the right associativity law"):
      forAll { (fa: F[A], f: A => F[B], g: B => F[C]) =>
        val lhs = fa >>= f >>= g
        val rhs = fa >>= (a => f(a) >>= (g))

        lhs shouldBe rhs
      }

  def checkAll[A, B, C](using
      Monad[F],
      Arbitrary[A],
      Arbitrary[F[A]],
      Arbitrary[A => F[B]],
      Arbitrary[B => F[C]],
      Equality[F[A]],
      Equality[F[B]],
      Equality[F[C]]
  ) =
    describe(s"$desc"):
      leftIdentity[A, B]
      rightIdentity[A]
      associativity[A, B, C]
