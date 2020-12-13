import aocd.Problem

object D05 extends Problem(2020, 5) :

  def run(input: List[String]): Unit =
    val seatIDs = input.map { (s: String) =>
      val row = Integer.parseInt(s.take(7).replace("F", "0").replace("B", "1"), 2)
      val col = Integer.parseInt(s.takeRight(3).replace("L", "0").replace("R", "1"), 2)
      row * 8 + col
    }.sorted
    println(seatIDs.max)
    println(seatIDs.zip(seatIDs.tail).map((a, b) => ((b - a), a)).dropWhile((d, _) => d == 1).head._2 + 1)
