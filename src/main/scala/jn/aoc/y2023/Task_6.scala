package jn.aoc.y2023

import jn.aoc.Utils

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Task_6 {

  case class Race(time: Long, recordDistance: Long)

  def part_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val timesAndRecords = source.getLines().map(_.split(":\\s+")(1).split("\\s+")).toList

      val races = timesAndRecords(0).indices.map { i =>
        val time = timesAndRecords(0)(i)
        val record = timesAndRecords(1)(i)
        Race(time.toInt, record.toInt)
      }

      races.map(race => calculatePossibleDistances(race).count(_ > race.recordDistance)).product
    }.get

  private def calculatePossibleDistances(race: Race): List[Long] =
    for {
      holdButtonMs <- (1L until race.time).toList
      travelMs = race.time - holdButtonMs
      speed = holdButtonMs
      distance = speed * travelMs
    } yield distance

  def part_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val timeAndRecord = source.getLines().map(_.split(":\\s+")(1)).toList

      val time = timeAndRecord(0).replace(" ", "").toLong
      val record = timeAndRecord(1).replace(" ", "").toLong
      val race = Race(time, record)

      countWinningWaysInConstMemory(1, race.time - 1, race.recordDistance, 0)
    }.get

  @tailrec
  private def countWinningWaysInConstMemory(speed: Long, travelTime: Long, recordDistance: Long, acc: Int): Int =
    if (travelTime == 0) {
      acc
    } else {
      val distance = speed * travelTime
      if (distance > recordDistance) {
        countWinningWaysInConstMemory(speed + 1, travelTime - 1, recordDistance, acc + 1)
      } else {
        countWinningWaysInConstMemory(speed + 1, travelTime - 1, recordDistance, acc)
      }
    }
}
