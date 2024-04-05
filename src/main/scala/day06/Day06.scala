package day06

object Day06 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val times = input.head.split(':')(1).split(' ').filter(_.nonEmpty).toList.map(_.toInt)
    val distances = input(1).split(':')(1).split(' ').filter(_.nonEmpty).toList.map(_.toInt)

    val p1 = times.indices.map(race =>
      (1 to times(race)).count(holdTime => holdTime * (times(race) - holdTime) > distances(race)))
      .product

    println("part 1: " + p1)

    val time = times.map(_.toString).reduce(_ ++ _).toInt
    val distance = distances.map(_.toString).reduce(_ ++ _).toLong
    val p2 = (1 to time).map(_.toLong).count(holdTime => holdTime * (time - holdTime) > distance)

    println("part 2: " + p2)
  }

}
