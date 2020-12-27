import mill._
import mill.scalalib._

object aoc20 extends ScalaModule {
  override def scalaVersion = "3.0.0-M3"

  override def ivyDeps = Agg(
    ivy"io.github.bbstilson::aocd:0.1.3".withDottyCompat(scalaVersion())
  )
}