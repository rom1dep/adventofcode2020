import aocd.Problem

object D01 extends Problem(2020, 1) :
  def run(input: List[String]): Unit =
    val parsed = input.map(_.toInt)
    println(part1(parsed))
    println(part2(parsed))

  def part1(input: List[Int]) =
    input.combinations(2).dropWhile(c => c.sum != 2020).next().product

  def part2(input: List[Int]) =
    input.combinations(3).dropWhile(c => c.sum != 2020).next().product