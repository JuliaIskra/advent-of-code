package jn.aoc.y2022

import scala.io.Source
import scala.util.Using

object Task_14 {
  private val sandSource = (500, 0)

  private def parseRockLocation(inputFile: String): Set[(Int, Int)] =
    Using(Source.fromFile(inputFile)) { source =>
      source
        .getLines()
        .foldLeft(Set[(Int, Int)]())((filledCells, line) =>
          val coordinates = line
            .split(" -> ")
            .map(coordinatePair =>
              val coordinateArray = coordinatePair.split(",")
              (coordinateArray(0).toInt, coordinateArray(1).toInt)
            )
          var updatedFilledCells = filledCells
          var coordIdx = 0
          while (coordIdx < coordinates.length - 1) {
            val from = coordinates(coordIdx)
            val to = coordinates(coordIdx + 1)
            if (from._1 == to._1) {
              val step = if (from._2 < to._2) 1 else -1
              (from._2 to to._2 by step).foreach(y => updatedFilledCells = updatedFilledCells + ((from._1, y)))
            } else {
              val step = if (from._1 < to._1) 1 else -1
              (from._1 to to._1 by step).foreach(x => updatedFilledCells = updatedFilledCells + ((x, from._2)))
            }
            coordIdx += 1
          }
          updatedFilledCells
        )
    }.get

  private def nextSandLocation(
      currentSandLocation: (Int, Int),
      isCellFilled: (Int, Int) => Boolean
  ): Option[(Int, Int)] =
    val (sandX, sandY) = currentSandLocation
    if (!isCellFilled(sandX, sandY + 1)) {
      Some((sandX, sandY + 1))
    } else if (!isCellFilled(sandX - 1, sandY + 1)) {
      Some((sandX - 1, sandY + 1))
    } else if (!isCellFilled(sandX + 1, sandY + 1)) {
      Some((sandX + 1, sandY + 1))
    } else {
      None
    }

  def part_1(inputFile: String): Int =
    var filledCells = parseRockLocation(inputFile)
    val minY = 0

    var sandComesToRest = true
    var counter = 0
    while (sandComesToRest) {
      val minX = filledCells.map(_._1).min
      val maxX = filledCells.map(_._1).max
      val maxY = filledCells.map(_._2).max

      var currentSandLocation = sandSource
      var rested = false
      while (
        !rested
        && minX <= currentSandLocation._1 && currentSandLocation._1 <= maxX
        && minY <= currentSandLocation._2 && currentSandLocation._2 <= maxY
      ) {
        val nextLocation =
          nextSandLocation(currentSandLocation, (x, y) => filledCells.contains((x, y)))
        if (nextLocation.isDefined) {
          currentSandLocation = nextLocation.get
        } else {
          rested = true
          filledCells = filledCells + currentSandLocation
          counter += 1
        }
      }
      sandComesToRest = rested
    }
    counter

  def part_2(inputFile: String): Int =
    var filledCells = parseRockLocation(inputFile)
    val maxY = filledCells.map(_._2).max + 2

    def isCellFilled(x: Int, y: Int): Boolean =
      filledCells.contains((x, y)) || y == maxY

    var lastSandLocation = (0, 0)
    var counter = 0
    while (lastSandLocation != sandSource) {
      var currentSandLocation = sandSource
      var rested = false
      while (!rested) {
        val nextLocation = nextSandLocation(currentSandLocation, isCellFilled)
        if (nextLocation.isDefined) {
          currentSandLocation = nextLocation.get
        } else {
          rested = true
          filledCells = filledCells + currentSandLocation
          counter += 1
        }
      }
      lastSandLocation = currentSandLocation
    }
    counter
}
