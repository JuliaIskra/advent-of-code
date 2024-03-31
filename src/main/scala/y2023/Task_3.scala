package y2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Task_3 {

  case class NumberWithCoord(n: Int, start: (Int, Int), end: (Int, Int))

  case class SymbolCoord(ch: Char, row: Int, col: Int)

  def part_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val (numbers, symbols) = source.getLines()
        .zipWithIndex
        .map((line, row) => parseLine(line, row, 0, List(), List()))
        .reduce { (a, b) =>
          val (na, sa) = a
          val (nb, sb) = b
          (na ++ nb, sa ++ sb)
        }

      numbers
        .filter(hasSymbolNearby(_, symbols.map(s => (s.row, s.col))))
        .map(_.n)
        .sum
    }.get


  @tailrec
  private def parseLine(line: String, row: Int, col: Int, numbers: List[NumberWithCoord], symbols: List[SymbolCoord]): (List[NumberWithCoord], List[SymbolCoord]) = {
    if (col < line.length) {
      val ch = line.charAt(col)
      if (ch.isDigit) {
        if (numbers.nonEmpty && numbers.last.end._2 == col - 1) {
          val lastN = numbers.last
          val newNumbers = numbers.take(numbers.length - 1) :+ NumberWithCoord((lastN.n.toString + ch).toInt, lastN.start, (row, col))
          parseLine(line, row, col + 1, newNumbers, symbols)
        } else {
          val newNumbers = numbers :+ NumberWithCoord(s"$ch".toInt, (row, col), (row, col))
          parseLine(line, row, col + 1, newNumbers, symbols)
        }
      } else if (ch != '.') {
        val newSymbols = symbols :+ SymbolCoord(ch, row, col)
        parseLine(line, row, col + 1, numbers, newSymbols)
      } else {
        parseLine(line, row, col + 1, numbers, symbols)
      }
    } else {
      (numbers, symbols)
    }
  }

  private def hasSymbolNearby(n: NumberWithCoord, symbols: List[(Int, Int)]): Boolean = {
    val (row, colStart) = n.start
    val (_, colEnd) = n.end

    val aroundStartCoord = Set(
      (row - 1, colStart - 1),
      (row - 1, colStart),
      (row, colStart - 1),
      (row + 1, colStart - 1),
      (row + 1, colStart)
    )

    val aroundEndCoord = Set(
      (row - 1, colEnd),
      (row - 1, colEnd + 1),
      (row, colEnd + 1),
      (row + 1, colEnd),
      (row + 1, colEnd + 1)
    )

    val aroundMiddleCoord = (colStart + 1 to colEnd)
      .flatMap(col => Set(
        (row - 1, col),
        (row + 1, col)
      ))

    (aroundStartCoord ++ aroundMiddleCoord ++ aroundEndCoord)
      .exists(symbols.contains)
  }
}