package fr.nate

case class Instruction(verb: String, n: Int)

case class Position(x: Int, y: Int, aim: Int)

object Day02 {
  def main(args: Array[String]): Unit = {
    val lines = Util.getLines("day02")
    val instructions = lines map { line =>
      val words = line split " "
      Instruction(words(0), words(1).toInt)
    }
    val arrival = instructions.foldLeft(Position(0, 0, 0)) { (pos, instr) =>
      instr match {
        case Instruction("forward", n) =>
          Position(pos.x + n, pos.y + (n * pos.aim), pos.aim)
        case Instruction("up", n)   => Position(pos.x, pos.y, pos.aim - n)
        case Instruction("down", n) => Position(pos.x, pos.y, pos.aim + n)
      }
    }
    println(arrival)
    println(arrival._1 * arrival._2)
  }
}
