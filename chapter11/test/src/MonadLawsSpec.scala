import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class OptionMonadLawsSpec extends MonadLaws[Option]("Option monad"):
  import MonadInstances.optionMonad

  checkAll[Int, Int, Int]

class ListMonadLawsSpec extends MonadLaws[List]("List monad"):
  import MonadInstances.listMonad

  checkAll[Int, Int, Int]

class IdMonadSpec extends MonadLaws[Id]("Id monad"):
  import Id.idMonad

  val genInt               = Gen.choose(-100, 100)
  given Arbitrary[Id[Int]] = Arbitrary(genInt.flatMap(Id(_)))

  checkAll[Int, Int, Int]
