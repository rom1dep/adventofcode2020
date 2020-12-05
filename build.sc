import mill._
import mill.scalalib._

object aoc20 extends ScalaModule {
  override def scalaVersion = "3.0.0-M2"

  override def ivyDeps = Agg(
    ivy"io.github.bbstilson::aocd:0.1.0".withDottyCompat(scalaVersion())
  )
}