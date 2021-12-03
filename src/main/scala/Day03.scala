package fr.nate

case class Counter(one: Int, zero: Int) {
  def higher: Int = if one >= zero then 1 else 0
  def lower: Int = if one >= zero then 0 else 1
}

case class RatingFinder(prefix: String, ratings: List[String]) {
  def index: Int = prefix.length
}

object Day03 {
  def binaryToInt(s: String): Int = {
    val chars = s.toCharArray.map({
      case '1' => 1
      case '0' => 0
    })
    chars.foldLeft(0) { (acc, n) => (acc * 2) + n }
  }

  def countChars(lines: List[String]): List[Counter] = {
    Range(0, lines.head.length).toList.map { idx =>
      lines.foldLeft(Counter(0, 0)) { (counter, line) =>
        line.charAt(idx) match {
          case '1' => Counter(counter.one + 1, counter.zero)
          case '0' => Counter(counter.one, counter.zero + 1)
        }
      }
    }
  }

  def findAirRating(
      lines: List[String]
  )(counterSelector: Counter => Int): Int = {
    val finder = Range(0, lines.head.length).foldLeft(RatingFinder("", lines)) {
      (finder, idx) =>
        if finder.ratings.length == 1 then {
          val finalRating = finder.ratings.head
          RatingFinder(finalRating, finder.ratings)
        } else {
          val counters = countChars(finder.ratings)
          val higher = counterSelector(counters(finder.index)).toString
          val newPrefix = finder.prefix + higher
          RatingFinder(
            newPrefix,
            lines filter {
              _ startsWith newPrefix
            }
          )
        }
    }
    binaryToInt(finder.prefix)
  }

  def main(args: Array[String]): Unit = {
    val lines = Util.getLines("day03")
    val counters = countChars(lines)
    val gamma = counters.foldLeft(0) { (n, counter) =>
      (2 * n) + counter.higher
    }
    val epsilon = counters.foldLeft(0) { (n, counter) =>
      (2 * n) + counter.lower
    }

    val oxygen = findAirRating(lines) { _.higher }
    val co2 = findAirRating(lines) { _.lower }

    println(oxygen * co2)
  }
}
