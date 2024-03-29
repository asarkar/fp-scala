import mill._, scalalib._, scalafmt._

trait FPModule extends ScalaModule with ScalafmtModule {
  // val baseDir = build.millSourcePath
  def scalaVersion = "3.3.1"

  def scalacOptions: T[Seq[String]] = Seq(
    "-encoding", "UTF-8",
    "-feature",
    "-Werror",
    "-explain",
    "-deprecation",
    "-unchecked",
    "-Wunused:all",
    "-rewrite",
    "-indent",
    "-source", "future",
  )
}

trait FpTestModule extends ScalaModule with TestModule.ScalaTest {
   def scalatestVersion = "3.2.17"
   def scalacheckVersion = "3.2.17.0"

   def ivyDeps = Agg(
      ivy"org.scalactic::scalactic:$scalatestVersion",
      ivy"org.scalatest::scalatest:$scalatestVersion",
    )
}

object chapter02 extends FPModule {
  object test extends FpTestModule with ScalaTests
}

object chapter03 extends FPModule {
  object test extends FpTestModule with ScalaTests
}

object chapter04 extends FPModule {
  object test extends FpTestModule with ScalaTests
}

object chapter05 extends FPModule {
  object test extends FpTestModule with ScalaTests
}

object chapter06 extends FPModule {
  object test extends FpTestModule with ScalaTests
}

object chapter10 extends FPModule {
  object test extends FpTestModule with ScalaTests {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scalatestplus::scalacheck-1-17:$scalacheckVersion"
    )
  }
}

object chapter11 extends FPModule {
  def moduleDeps = Seq(chapter06)
  def scalacOptions = super.scalacOptions() ++ Agg(
    "-Ykind-projector:underscores"
  )

  object test extends FpTestModule with ScalaTests {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scalatestplus::scalacheck-1-17:$scalacheckVersion"
    )
  }
}

object chapter12 extends FPModule {
  def moduleDeps = Seq(chapter06, chapter10)
  def scalacOptions = super.scalacOptions() ++ Agg(
    "-Ykind-projector:underscores"
  )
  object test extends FpTestModule with ScalaTests {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scalatestplus::scalacheck-1-17:$scalacheckVersion"
    )
  }
}
