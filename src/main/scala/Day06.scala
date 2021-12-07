package fr.nate

case class Fishes(fishes: Map[Int, Long]) {
  def tick: Fishes = {
    val tickedFishes = fishes map { case (k, v) => k - 1 -> v }
    val fishesToReset = tickedFishes.getOrElse(-1, 0L)
    Fishes(
      tickedFishes.flatMap({
        case (-1, _) => List()
        case kv      => List(kv)
      }) ++ List(
        8 -> (tickedFishes.getOrElse(8, 0L) + fishesToReset),
        6 -> (tickedFishes.getOrElse(6, 0L) + fishesToReset)
      )
    )
  }
}

object Day06 {
  def main(args: Array[String]): Unit = {
    val input = Util.getLines("day06").head

    val fishes = input
      .split(",")
      .map(_.toInt)
      .groupBy(i => i)
      .map(kv => kv._1 -> kv._2.length.toLong)

    val ticked = Range(0, 256).foldLeft(Fishes(fishes)) { (fishes, _) =>
      println(fishes)
      fishes.tick
    }
    println(fishes)
    println(ticked.fishes.values.sum)
  }
}
