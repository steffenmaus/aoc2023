package day01

object Day01 {

  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val a = input
      .map(l => l.filter(_.isDigit))
      .map(l => l.head.asDigit * 10 + l.last.asDigit)
      .sum
    println("part 1: " + a)

    val b = input
      .map(l => trimString(l, ""))
      .map(l => l.head.asDigit * 10 + l.last.asDigit)
      .sum
    println("part 2: " + b)

  }

  def trimString(s: String, t: String): String = {
    if (s.isEmpty) {
      return t
    }
    if (s.head.isDigit) {
      return trimString(s.drop(1), t.appended(s.head))
    }
    if (s.startsWith("one")) return trimString(s.drop(1), t.appended('1'))
    if (s.startsWith("two")) return trimString(s.drop(1), t.appended('2'))
    if (s.startsWith("three")) return trimString(s.drop(1), t.appended('3'))
    if (s.startsWith("four")) return trimString(s.drop(1), t.appended('4'))
    if (s.startsWith("five")) return trimString(s.drop(1), t.appended('5'))
    if (s.startsWith("six")) return trimString(s.drop(1), t.appended('6'))
    if (s.startsWith("seven")) return trimString(s.drop(1), t.appended('7'))
    if (s.startsWith("eight")) return trimString(s.drop(1), t.appended('8'))
    if (s.startsWith("nine")) return trimString(s.drop(1), t.appended('9'))

    trimString(s.drop(1), t)
  }

}
