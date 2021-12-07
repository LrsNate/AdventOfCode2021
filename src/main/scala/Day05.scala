package fr.nate

case class Line(start: (Int, Int), end: (Int, Int)) {}

case class Crossings(cells: Map[(Int, Int), Int]) {
  def addLine(line: Line): Crossings = line match {
    case Line((startX, startY), (endX, endY)) =>
      val xRange =
        if startX == endX then
          Range.inclusive(startY, endY, if startY < endY then 1 else -1) map {
            _ =>
              startX
          }
        else Range.inclusive(startX, endX, if startX < endX then 1 else -1)
      val yRange =
        if startY == endY then
          Range.inclusive(startX, endX, if startX < endX then 1 else -1) map {
            _ =>
              startY
          }
        else Range.inclusive(startY, endY, if startY < endY then 1 else -1)
      xRange.zip(yRange).foldLeft(this) { (crossings, coord) =>
        crossings.addCell(coord._1, coord._2)
      }
  }

  def addCell(x: Int, y: Int): Crossings = {
    val value = cells.getOrElse((x, y), 0)
    Crossings(cells.updated((x, y), value + 1))
  }
}

object Day05 {
  def parseLine(line: String): Line = {
    val pattern = raw"(\d+),(\d+) -> (\d+),(\d+)".r
    line match {
      case pattern(startX, startY, endX, endY) =>
        Line((startX.toInt, startY.toInt), (endX.toInt, endY.toInt))
      case _ => throw IllegalArgumentException()
    }
  }
  def main(args: Array[String]): Unit = {
    val lines = Util.getLines("day05")
    val parsedLines: List[Line] = lines.map(parseLine)
    val crossings = parsedLines.foldLeft(Crossings(Map())) {
      (crossings, line) => crossings.addLine(line)
    }
    println(crossings.cells.filter((k, v) => v >= 2).size)
  }
}
