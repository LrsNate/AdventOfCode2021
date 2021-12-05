package fr.nate

case class Cell(n: Int, marked: Boolean)

case class Board(rows: List[List[Cell]]) {
  def isWinning: Boolean = {
    val rowsWin = rows exists { _ forall { _.marked } }
    val columnsWin = rows.head.indices.map({ idx =>
      rows map { row => row(idx) }
    }) exists { _ forall { _.marked } }

    rowsWin || columnsWin
  }

  def mark(n: Int): Board = {
    val newCells = rows map {
      _ map { cell => Cell(cell.n, cell.marked || cell.n == n) }
    }
    Board(newCells)
  }

  def getScore: Int = rows.map({ _.filter(c => !c.marked).map(_.n).sum }).sum
}

class BoardParsingState(val boards: List[Board]) {
  def parseLine(line: String): BoardParsingState = line match {
    case "" => BoardParsingState(boards :+ Board(List()))
    case _ =>
      val row = line
        .split(" ")
        .filter(line => !line.isBlank)
        .map(n => Cell(n.toInt, false))
        .toList
      BoardParsingState(withNewRow(row)(boards))
  }

  def withNewRow(row: List[Cell]): List[Board] => List[Board] = {
    case Nil                => Nil
    case Board(rows) :: Nil => List(Board(rows :+ row))
    case head :: tail       => head :: withNewRow(row)(tail)
  }
}

object Day04 {
  def parseBoards(lines: List[String]): List[Board] = lines
    .foldLeft(BoardParsingState(List()))((state, line) => state parseLine line)
    .boards

  def play(
      boards: List[Board],
      winningBoards: List[Board]
  ): List[Int] => Int = {
    case Nil => throw IllegalArgumentException()
    case n :: tail =>
      val updatedBoards = boards map { _ mark n }
      val newlyWinningBoards = updatedBoards filter { _.isWinning }
      val remainingBoards = updatedBoards filter { board => !board.isWinning }
      if remainingBoards.isEmpty then n * newlyWinningBoards.last.getScore
      else play(remainingBoards, winningBoards ++ newlyWinningBoards)(tail)
  }

  def main(args: Array[String]): Unit = {
    val lines = Util.getLines("day04")
    val (boardLines, instructions) = lines match {
      case head :: tail => (tail, head.split(",").map(_.toInt).toList)
      case Nil          => throw IllegalArgumentException()
    }
    val boards = parseBoards(boardLines)
    println(play(boards, List())(instructions))
  }
}
