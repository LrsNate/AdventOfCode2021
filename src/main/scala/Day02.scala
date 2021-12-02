package fr.nate

object Day02 {
  def main(args: Array[String]): Unit = {
    val lines = Util.getLines("day02")
    val instructions = lines map { line =>
      val words = line split " "
      (words(0), words(1).toInt)
    }
    val arrival = instructions.foldLeft((0, 0, 0)) { (pos, instr) =>
      instr match {
        case ("forward", n) => (pos._1 + n, pos._2 + (n * pos._3), pos._3)
        case ("up", n) => (pos._1, pos._2, pos._3 - n)
        case ("down", n) => (pos._1, pos._2, pos._3 + n)
      }
    }
    println(arrival)
    println(arrival._1 * arrival._2)
  }
}
