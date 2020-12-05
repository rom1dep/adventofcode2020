import aocd.Problem

object D04 extends Problem(2020, 4) :

  def run(input: List[String]): Unit =
    val passports = input.mkString("\n").replace("\n\n", ";").replace("\n", " ").split(";").toList
    val part1Valid = passports.filter(part1Validator)
    println(part1Valid.size)
    val part2Valid = part1Valid.filter(part2Validator)
    println(part2Valid.size)

  def part1Validator(passport: String): Boolean =
    val mandatory = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    mandatory.forall(m => passport.contains(m + ":"))

  val dataPattern = raw"(\w{3}):(\S+)".r
  val colorPattern = raw"#[0-9a-f]{6}".r
  val idPattern = raw"\d{9}".r
  def part2Validator(passport: String): Boolean =
    val data = dataPattern.findAllMatchIn(passport).map(m => m.group(1) -> m.group(2)).toMap
    (1920 to 2002).contains(data("byr").toInt) && // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    (2010 to 2020).contains(data("iyr").toInt) && // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    (2020 to 2030).contains(data("eyr").toInt) && // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    { // hgt (Height) - a number followed by either cm or in:
      //   If cm, the number must be at least 150 and at most 193.
      //   If in, the number must be at least 59 and at most 76.
      val hgt = data("hgt"); val h = hgt.dropRight(2).toInt
      if hgt.endsWith("cm") then (150 to 193).contains(h) else (59 to 76).contains(h) } &&
    colorPattern.matches(data("hcl")) && // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    "amb blu brn gry grn hzl oth".split(" ").contains(data("ecl")) && // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    idPattern.matches(data("pid")) // pid (Passport ID) - a nine-digit number, including leading zeroes.
    // cid (Country ID) - ignored, missing or not.