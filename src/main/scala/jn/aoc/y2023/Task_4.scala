package jn.aoc.y2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Task_4 {

  def part_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source
        .getLines()
        .map { line =>
          val numberSets = line.split(":")(1).split('|')
          val winningNumbers = numberSets(0).split(" ").filterNot(_.isEmpty).map(_.toInt)
          val numbers = numberSets(1).split(" ").filterNot(_.isEmpty).map(_.toInt)
          val matchCount = numbers.count(winningNumbers.contains)
          Math.pow(2, matchCount - 1).toInt
        }
        .sum
    }.get

  def part_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val cardToMatches = source
        .getLines()
        .map { line =>
          val numberSets = line.split(":")
          val cardId = numberSets(0).split("\\s+")(1).toInt
          val winningNumbers = numberSets(1).split('|')(0).split(" ").filterNot(_.isEmpty).map(_.toInt)
          val numbers = numberSets(1).split('|')(1).split(" ").filterNot(_.isEmpty).map(_.toInt)
          val matchCount = numbers.count(winningNumbers.contains)
          (cardId, matchCount)
        }
        .toMap

      val allCards = gatherCopyCards(cardToMatches.keys.toList.sorted, cardToMatches, Map())
      allCards.values.sum
    }.get

  @tailrec
  private def gatherCopyCards(
      cards: List[Int],
      cardToMatches: Map[Int, Int],
      cardsToCount: Map[Int, Int]
  ): Map[Int, Int] = {
    if (cards.isEmpty) {
      cardsToCount
    } else {
      val currentCardId :: rest = cards: @unchecked
      val updatedCardsToCount = scala.collection.mutable.Map.from(cardsToCount)
      val currentCardCount = cardsToCount.getOrElse(currentCardId, 0) + 1
      updatedCardsToCount.update(currentCardId, currentCardCount)

      if (cardToMatches(currentCardId) > 0) {
        (currentCardId + 1 to currentCardId + cardToMatches(currentCardId))
          .foreach { copyCardId =>
            val copyCardCount = cardsToCount.getOrElse(copyCardId, 0) + currentCardCount
            updatedCardsToCount.update(copyCardId, copyCardCount)
          }
        gatherCopyCards(rest, cardToMatches, updatedCardsToCount.toMap)
      } else {
        gatherCopyCards(rest, cardToMatches, updatedCardsToCount.toMap)
      }
    }
  }
}
