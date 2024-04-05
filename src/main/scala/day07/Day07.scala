package day07

object Day07 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  lazy val CARD_VALUES: Map[Char, Int] = "23456789TJQKA".toList.zipWithIndex.toMap
  lazy val CARD_VALUES_2: Map[Char, Int] = "J23456789TQKA".toList.zipWithIndex.toMap
  lazy val REAL_CARDS : Set[Char] = CARD_VALUES_2.keySet.filter(!_.equals('J'))

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val sum = f(input, ordering)
    println("part 1: " + sum)

    val sum2 = f(input, ordering2)
    println("part 2: " + sum2)
  }

  def f(in: Array[String], ord: ((String, Int), (String, Int)) => Boolean): Int = {
    in
      .map(_.split(' '))
      .map(t => (t(0), t(1).toInt))
      .sortWith(ord)
      .reverse
      .zipWithIndex
      .map(x => x._1._2 * (x._2 + 1))
      .sum
  }

  def ordering(t1: (String, Int), t2: (String, Int)): Boolean = {
    val po1 = primaryScore(t1._1)
    val po2 = primaryScore(t2._1)
    if (po1 > po2) {
      return true
    } else if (po1 < po2) {
      return false
    }
    secondaryOrder(t1._1, t2._1, CARD_VALUES)
  }

  def ordering2(t1: (String, Int), t2: (String, Int)): Boolean = {
    val po1 = REAL_CARDS.map(x => primaryScore(t1._1.replace('J', x))).max
    val po2 = REAL_CARDS.map(x => primaryScore(t2._1.replace('J', x))).max
    if (po1 > po2) {
      return true
    } else if (po1 < po2) {
      return false
    }
    secondaryOrder(t1._1, t2._1, CARD_VALUES_2)
  }

  def primaryScore(in: String): Int = {
    in.toSet.size match {
      case 1 => 6 //five of a kind
      case 2 => if (List(1, 4).contains(in.count(_.equals(in.head)))) 5 else 4 //four of a kind / full house
      case 3 => if (in.count(_.equals(in.head)) == 3 || in.count(_.equals(in(1))) == 3 || in.count(_.equals(in(2))) == 3) 3 else 2 //three of a kind / double pair
      case 4 => 1 //pair
      case _ => 0 //high card
    }
  }

  def secondaryOrder(in1: String, in2: String, m: Map[Char, Int]): Boolean = {
    val replaced1 = in1.map(m)
    val replaced2 = in2.map(m)

    for (i <- replaced1.indices) {
      if (replaced1(i) > replaced2(i)) {
        return true
      } else if (replaced1(i) < replaced2(i)) {
        return false
      }
    }
    false
  }

}
