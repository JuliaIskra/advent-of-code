package jn.aoc.y2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Task_1 {

  def part_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines.map { line =>
        val digits = line.filter(_.isDigit)
        (digits.take(1) + digits.takeRight(1)).toInt
      }.sum
    }.get

  def part_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines.map { line =>
        val digits = findDigits(line, 0, 1, List())
        digits.head * 10 + digits.last
      }.sum
    }.get

  @tailrec
  private def findDigits(line: String, start: Int, end: Int, digitsAcc: List[Int]): List[Int] = {
    if (start < line.length && end <= line.length) {
      val w = line.substring(start, end)
      if (w.length == 1 && w.matches("\\d")) {
        val newStart = start + 1
        val newEnd = Math.max(end, newStart + 1)
        findDigits(line, newStart, newEnd, digitsAcc.appended(w.toInt))
      } else {
        val possibleWords = wordToDigit.keys.filter(_.startsWith(w))
        if (possibleWords.nonEmpty) {
          if (possibleWords.size == 1 && possibleWords.head == w) {
            val newStart = start + 1
            val newEnd = Math.max(end, newStart + 1)
            findDigits(line, newStart, newEnd, digitsAcc.appended(wordToDigit(w)))
          } else {
            findDigits(line, start, end + 1, digitsAcc)
          }
        } else {
          val newStart = start + 1
          val newEnd = Math.max(end, newStart + 1)
          findDigits(line, newStart, newEnd, digitsAcc)
        }
      }
    } else digitsAcc
  }

  private val wordToDigit = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )
}
