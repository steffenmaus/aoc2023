package day05

object Day05 {

  lazy val fileInput: Array[Array[String]] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n\n").map(_.split("\n"))
  lazy val testInput: Array[Array[String]] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n\n").map(_.split("\n"))

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val seeds = input.head.head.split(':')(1).split(' ').filter(_.nonEmpty).map(_.toLong).toList
    val mappings = input.drop(1).map(_.drop(1).toList.map(_.split(' ').filter(_.nonEmpty).map(_.toLong).toList)).toList

    var current = seeds
    var current2 = seeds.zip(seeds.tail).zipWithIndex.filter(_._2 % 2 == 0).map(_._1).map(t => (t._1, t._1 + t._2 - 1))
    for (m <- mappings) {
      current = mapper(current, m)
      current2 = mapper2(current2, m)
    }

    println("part 1: " + current.min)
    println("part 2: " + current2.map(_._1).min)

  }

  def mapper2(in: List[(Long, Long)], mapping: List[List[Long]]): List[(Long, Long)] = {
    in.flatMap(temp => {
      val m = mapping.filter(rule => (rule(1) <= temp._1) && (rule(1) + rule(2) > temp._1))
      if (m.isEmpty) {
        List(temp)
      } else {
        val offset = m.head(0) - m.head(1)
        val rangeLength = m.head(2)
        if (temp._2 <= m.head(1) + rangeLength - 1) {
          List((temp._1 + offset, temp._2 + offset))
        } else {
          List((temp._1 + offset, m.head(0) + rangeLength - 1)).appendedAll(mapper2(List((m.head(1) + rangeLength, temp._2)), mapping))
        }
      }
    })
  }

  def mapper(in: List[Long], mapping: List[List[Long]]): List[Long] = {
    in.map(temp => {
      val m = mapping.filter(rule => (rule(1) <= temp) && (rule(1) + rule(2) > temp))
      if (m.nonEmpty) {
        temp + (m.head(0) - m.head(1))
      } else {
        temp
      }
    })
  }
}
