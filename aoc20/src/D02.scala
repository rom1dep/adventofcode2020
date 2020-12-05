import aocd.Problem

object D02 extends Problem(2020, 2) :
  def run(input: List[String]): Unit = {
    println(part1(input))
    println(part2(input))
  }

  val pattern = raw"(\d+)-(\d+) (\w): (\w+)".r

  def part1(input: List[String]) =
    input.filter(_ match { case pattern(lo, up, chr, pass) =>
      val charCount = pass.filter(_ == chr.toList.head).size
      lo.toInt <= charCount && charCount <= up.toInt
    }).length

  def part2(input: List[String]) =
    input.filter(_ match { case pattern(li, ri, chr, pass) =>
      (pass.charAt(li.toInt - 1) == chr.toList.head) ^ (pass.charAt(ri.toInt - 1) == chr.toList.head)
    }).length