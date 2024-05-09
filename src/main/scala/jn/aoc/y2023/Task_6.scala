package jn.aoc.y2023

import jn.aoc.Utils

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Task_6 {

  case class Race(time: Int, recordDistance: Int)

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

  private def calculatePossibleDistances(race: Race): List[Int] =
    for {
      holdButtonMs <- (1 to race.time).toList
      travelMs = race.time - holdButtonMs
      speed = holdButtonMs
      distance = speed * travelMs
    } yield distance
}
