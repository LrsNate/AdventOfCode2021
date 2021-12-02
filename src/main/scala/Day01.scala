package fr.nate

import scala.io.Source

object Day01 {
  def main(args: Array[String]): Unit = {
    val depths = Util.getLines("day01") map { _.toInt }
    val windows = Range(2, depths.size).map { idx =>
      depths(idx - 2) + depths(idx - 1) + depths(idx)
    }
    println(
      Range(1, windows.size)
        .map({ (idx: Int) =>
          if windows(idx - 1) < windows(idx) then 1 else 0
        })
        .sum
    )
  }
}
