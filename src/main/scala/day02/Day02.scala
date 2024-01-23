package day02

object Day02 {

  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val a = input
      .map(_.split(':')(1))
      .map(_.split(';').filter(griff => {
        griff.split(',')
          .exists(x => {
            (x.contains("red") && x.split(' ')(1).toInt > 12) ||
              (x.contains("green") && x.split(' ')(1).toInt > 13) ||
              (x.contains("blue") && x.split(' ')(1).toInt > 14)
          })
      })).zipWithIndex.map(x => (x._1, x._2 + 1))
      .filter(_._1.isEmpty)
      .map(_._2)
      .sum

    println("part 1: " + a)


    val b = input
      .map(_.split(':')(1))
      .map(game => {
        val s = game.split(';').flatMap(_.split(','))
        val maxRot = s.filter(_.contains("red")).map(_.split(' ')(1).toInt).max
        val maxGruen = s.filter(_.contains("green")).map(_.split(' ')(1).toInt).max
        val maxBlau = s.filter(_.contains("blue")).map(_.split(' ')(1).toInt).max
        maxRot * maxGruen * maxBlau
      })
      .sum
    println("part 2: " + b)

  }

}
