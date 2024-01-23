package day03

import scala.collection.mutable

object Day03 {

  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val symbols: mutable.Map[(Int, Int), Char] = mutable.HashMap.empty
    for (y <- input.indices) {
      for (x <- input.head.indices) {
        if (!input(y)(x).isDigit && !input(y)(x).equals('.')) {
          symbols.addOne(((x, y), input(y)(x)))
        }
      }
    }

    var sum = 0
    val digits: mutable.Set[(Set[(Int, Int)], Int)] = mutable.HashSet.empty

    for (y <- input.indices) {
      var currentNumber = 0
      val currentNumberCoords: mutable.Set[(Int, Int)] = mutable.HashSet.empty
      for (x <- input.head.indices) {
        if (input(y)(x).isDigit) {
          currentNumber = currentNumber * 10 + input(y)(x).asDigit
          currentNumberCoords.add((x, y))
        } else {
          if (currentNumber != 0) {
            if (checkNeighbours(currentNumberCoords, symbols.keySet)) {
              sum = sum + currentNumber
            }
            digits.addOne((currentNumberCoords.toSet, currentNumber))
            currentNumber = 0
            currentNumberCoords.clear()
          }
        }
      }
      if (currentNumber != 0) { //handle end of line digits
        if (checkNeighbours(currentNumberCoords, symbols.keySet)) {
          sum = sum + currentNumber
        }
        digits.addOne((currentNumberCoords.toSet, currentNumber))
      }
    }

    println("part 1: " + sum)

    val x = symbols
      .filter(_._2.equals('*'))
      .map(s => getNeighbourValues(s._1, digits))
      .filter(_.size == 2)
      .map(l => l.head * l.last)
      .sum
    println("part 2: " + x)

  }

  def getNeighbourValues(tuple: (Int, Int), digits: mutable.Set[(Set[(Int, Int)], Int)]): List[Int] = {
    val neighbours: mutable.Set[(Set[(Int, Int)], Int)] = mutable.HashSet.empty

    for (d <- digits) {
      for (c <- d._1) {
        if (
          tuple.equals((c._1 - 1, c._2)) ||
            tuple.equals((c._1, c._2 - 1)) ||
            tuple.equals((c._1 + 1, c._2)) ||
            tuple.equals((c._1, c._2 + 1)) ||
            tuple.equals((c._1 - 1, c._2 - 1)) ||
            tuple.equals((c._1 + 1, c._2 + 1)) ||
            tuple.equals((c._1 - 1, c._2 + 1)) ||
            tuple.equals((c._1 + 1, c._2 - 1))
        ) {
          neighbours.add(d)
        }
      }
    }
    neighbours.toList.map(_._2)
  }

  def checkNeighbours(set1: mutable.Set[(Int, Int)], set2: scala.collection.Set[(Int, Int)]): Boolean = {
    for (c <- set1) {
      if (
        set2.contains((c._1, c._2 + 1)) ||
          set2.contains((c._1, c._2 - 1)) ||
          set2.contains((c._1 + 1, c._2)) ||
          set2.contains((c._1 - 1, c._2)) ||
          set2.contains((c._1 - 1, c._2 - 1)) ||
          set2.contains((c._1 + 1, c._2 + 1)) ||
          set2.contains((c._1 - 1, c._2 + 1)) ||
          set2.contains((c._1 + 1, c._2 - 1))
      ) {
        return true
      }
    }
    false
  }

}
