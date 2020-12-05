import aocd.Problem

object D03 extends Problem(2020, 3) :

  def run(input: List[String]): Unit =
    println(treesForTrajectory(input))
    println(List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map((stepX, stepY) => treesForTrajectory(input, stepX, stepY)).product)

  def treesForTrajectory(input: List[String], stepX: Int = 3, stepY: Int = 1) =
    val width = input(0).length
    input.grouped(stepY).foldLeft((0, 0)) { case ((treesCnt: Int, pos: Int), row: List[String]) =>
      row.head.toList(pos % width) match
        case '#' => (treesCnt + 1, pos + stepX)
        case _ => (treesCnt, pos + stepX)
    }._1