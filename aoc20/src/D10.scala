import aocd.Problem

import scala.annotation.tailrec

object D10 extends Problem(2020, 10) :

  def run(input: List[String]): Unit =
    val adapters = input.map(_.toInt).sorted

    sol1(adapters)
    sol2(adapters)

  def sol1(adapters: List[Int]) = part1 (
    adapters.zip(adapters.tail).map((a, b) => b - a).groupBy(identity).map(_._2.length + 1).product
  )

  def sol2(adapters: List[Int]) = part2 {
    @tailrec def connectAt(availAdapters: List[Int] = adapters, combAt: Map[Int, Long] = Map(0 -> 1)): Map[Int, Long] =
      if availAdapters.isEmpty then combAt else
        val currJolt = availAdapters.head
        val currCombo = combAt.getOrElse(currJolt - 1, 0L) +
          combAt.getOrElse(currJolt - 2, 0L) +
          combAt.getOrElse(currJolt - 3, 0L)
        connectAt(availAdapters.tail, combAt.updated(currJolt, currCombo))

    connectAt()(adapters.max)
  }
