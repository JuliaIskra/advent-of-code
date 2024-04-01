package y2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Task_4 {

  def part_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source
        .getLines()
        .map { line =>
          val a = line.split(":")(1).split('|')
          val winningNumbers = a(0).split(" ").filterNot(_.isEmpty).map(_.toInt)
          val numbers = a(1).split(" ").filterNot(_.isEmpty).map(_.toInt)
          val matchCount = numbers.count(winningNumbers.contains)
          Math.pow(2, matchCount - 1).toInt
        }
        .sum
    }.get

  def part_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source
        .getLines()
        .map { line =>
          val a = line.split(":")
          val cardId = a(0).split(" ")(1).toInt
          val winningNumbers = a(1).split('|')(0).split(" ").filterNot(_.isEmpty).map(_.toInt)
          val numbers = a(1).split('|')(1).split(" ").filterNot(_.isEmpty).map(_.toInt)
          val matchCount = numbers.count(winningNumbers.contains)
          (cardId, matchCount)
        }
        .size
    }.get
}
