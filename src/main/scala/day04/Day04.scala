package day04

import scala.collection.mutable

object Day04 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")


  def main(args: Array[String]): Unit = {
    val input = fileInput

    val matchingNumbers = input
      .map(_.split(':')(1))
      .map(_.split('|'))
      .map(sl => (sl(0), sl(1)))
      .map(tuple => (tuple._1.split(' ').toSet.filter(_.nonEmpty).map(_.toInt), tuple._2.split(' ').toSet.filter(_.nonEmpty).map(_.toInt)))
      .map(t => t._1.intersect(t._2).size)

    val a = matchingNumbers
      .filter(_ > 0)
      .map(x => Math.pow(2, x - 1).toInt)
      .sum

    println("part 1: " + a)

    val matchingNumbersIndexed = matchingNumbers
      .zipWithIndex
      .map(_.swap)
      .toList

    val cardValues = matchingNumbersIndexed.toMap
    val cardCounters: mutable.Map[Int, Int] = mutable.HashMap.empty
    cardValues.keys.foreach(cardCounters.addOne(_, 1))

    for (i <- matchingNumbersIndexed.indices) {
      val mult = cardCounters(i)
      for (offset <- cardValues(i) until 0 by -1) {
        val oldVal = cardCounters(i + offset)
        cardCounters.update(i + offset, oldVal + mult)
      }
    }

    println("part 2: " + cardCounters.toList.map(_._2).sum)

  }

}
