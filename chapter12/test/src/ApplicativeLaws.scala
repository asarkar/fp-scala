import org.scalacheck.Arbitrary
import org.scalactic.Equality
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers.shouldBe

trait ApplicativeLaws[F[_]](desc: String) extends ScalaCheckPropertyChecks with AnyFunSpecLike:
  def functorIdentity[A](using
      Applicative[F],
      Arbitrary[A],
      Arbitrary[F[A]],
      Equality[F[A]]
  ): Unit =
    it("should satisfy the Functor identity law"):
      forAll { (fa: F[A]) =>
        fa.map(identity) shouldBe fa
      }

  def functorComposition[A, B, C](using
      Applicative[F],
      Arbitrary[F[A]],
      Arbitrary[A => B],
      Arbitrary[B => C],
      Equality[F[C]]
  ): Unit =
    it("should satisfy the Functor composition law"):
      forAll { (fa: F[A], f: A => B, g: B => C) =>
        fa.map(f).map(g) shouldBe fa.map(f.andThen(g))
      }

  def leftIdentity[A](using
      Applicative[F],
      Arbitrary[A],
      Arbitrary[F[A]],
      Equality[F[A]]
  ): Unit =
    it("should satisfy the left identity law"):
      forAll { (fa: F[A]) =>
        val af = summon[Applicative[F]]
        af.unit(()).map2(fa)((_, a) => a) shouldBe fa
      }

  def rightIdentity[A](using
      Applicative[F],
      Arbitrary[A],
      Arbitrary[F[A]],
      Equality[F[A]]
  ): Unit =
    it("should satisfy the right identity law"):
      forAll { (fa: F[A]) =>
        val af = summon[Applicative[F]]
        fa.map2(af.unit(()))((a, _) => a) shouldBe fa
      }

  def associativity[A, B, C](using
      Applicative[F],
      Arbitrary[F[A]],
      Arbitrary[F[B]],
      Arbitrary[F[C]],
      Equality[F[A]],
      Equality[F[B]],
      Equality[F[C]]
  ): Unit =
    it("should satisfy the associativity law"):
      forAll { (fa: F[A], fb: F[B], fc: F[C]) =>
        val af = summon[Applicative[F]]

        val lhs: F[((A, B), C)] = fa.product(fb).product(fc)
        val rhs: F[((A, B), C)] = fa.product(fb.product(fc)).map(af.assoc)

        lhs shouldBe rhs
      }

  def naturality[A, B, C](using
      Applicative[F],
      Arbitrary[F[A]],
      Arbitrary[F[B]],
      Arbitrary[A => B],
      Arbitrary[B => C]
  ): Unit =
    it("should satisfy the naturality law"):
      forAll { (fa: F[A], fb: F[B], f: A => B, g: B => C) =>
        fa.map2(fb)((a, b) => (f(a), g(b))) == fa.map(f).product(fb.map(g))
      }

  def checkAll[A, B, C](using
      Applicative[F],
      Arbitrary[A],
      Arbitrary[F[A]],
      Arbitrary[F[B]],
      Arbitrary[F[C]],
      Arbitrary[A => B],
      Arbitrary[B => C],
      Equality[F[A]],
      Equality[F[B]],
      Equality[F[C]]
  ) =
    describe(s"$desc"):
      functorIdentity[A]
      functorComposition[A, B, C]
      leftIdentity[A]
      rightIdentity[A]
      associativity[A, B, C]
      naturality[A, B, C]
