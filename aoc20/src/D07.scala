import aocd.Problem
import scala.annotation.tailrec

object D07 extends Problem(2020, 7) :
  val outerRgx = "^(.*) bags contain (.*)$".r("container", "containee")
  val innerRgx = "(\\d+) (.*?) bag".r.unanchored

  def run(input: List[String]): Unit = {
    // creates a map that, given a bag (string), returns the bags that contain it
    val parents: Map[String, List[String]] = input.flatMap(_ match
      case outerRgx(container, containee) =>
        for (m <- innerRgx.findAllMatchIn(containee))
          yield m.group(2) -> container
    ).groupMap(_._1)(_._2)
    sol1(parents)

    // creates a map that, given a bag (string), returns its content
    val contents: Map[String, List[(String, Int)]] = input.flatMap(_ match
      case outerRgx(container, containee) =>
        for (m <- innerRgx.findAllMatchIn(containee))
          yield container -> (m.group(2), m.group(1).toInt)
    ).groupMap(_._1)(_._2)
    sol2(contents)
  }

  def sol1(parents: Map[String, List[String]]): Int = part1 {
    @tailrec def traverse(node: List[String] = List("shiny gold"), seen: Set[String] = Set.empty): Set[String] = node match
      case head :: tail =>
        if parents.contains(head) then traverse(parents(head) ::: tail, seen ++ Set(head))
        else traverse(tail, seen ++ Set(head))
      case Nil => seen

    traverse().size - 1
  }

  def sol2(contents: Map[String, List[(String, Int)]]): Int = part2 {
    def traverse(current: String): Int = contents.get(current) match
      case Some(bags) => bags.map { bag =>
        val thisBag = bag._2
        val otherBags = thisBag * traverse(bag._1)
        thisBag + otherBags
      }.sum
      case None => 0

    traverse("shiny gold")
  }
