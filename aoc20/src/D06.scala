import aocd.Problem

object D06 extends Problem(2020, 6) :

  def run(input: List[String]): Unit = {
    val groups = input.mkString("\n").split("\n\n").toList
    sol1(groups)
    sol2(groups)
  }

  def sol1(input: List[String]): Int = part1(
    input.map(g => (g.toSet - '\n').size).sum
  )

  def sol2(input: List[String]): Int = part2(
    input.map(g => g.split("\n") // in each group
      .map(_.toSet) // for each person, put the answer in a set
      .fold(('a' to 'z').toSet)(_.intersect(_)) // intersect all answers from all persons
      .size // that gives the number of common answers for this group
    ).sum
  )
