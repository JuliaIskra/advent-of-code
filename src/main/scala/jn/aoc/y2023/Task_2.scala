package jn.aoc.y2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Task_2 {

  case class CubeSet(red: Int, green: Int, blue: Int)

  private val gameIdPattern = "Game (\\d+)".r
  private val redPattern = " (\\d+) red".r
  private val greenPattern = " (\\d+) green".r
  private val bluePattern = " (\\d+) blue".r

  def part_1(inputFile: String, redTotal: Int, greenTotal: Int, blueTotal: Int): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .map(parseGame)
        .filter { (gameId, cubeSets) =>
          cubeSets.forall { set =>
            set.red <= redTotal && set.green <= greenTotal && set.blue <= blueTotal
          }
        }
        .map(_._1)
        .sum
    }.get

  def part_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .map(parseGame)
        .map { (gameId, cubeSets) =>
          val maxRed = cubeSets.map(_.red).max
          val maxGreen = cubeSets.map(_.green).max
          val maxBlue = cubeSets.map(_.blue).max
          maxRed * maxGreen * maxBlue
        }
        .sum
    }.get

  private def parseGame(line: String) = {
    val a = line.split(":")

    val gameId = gameIdPattern
      .findAllIn(a(0))
      .matchData
      .map(_.group(1))
      .next()
      .toInt

    val sets = a(1)
      .split(";")
      .map(parseCubeSet)

    gameId -> sets
  }

  private def parseCubeSet(cubeSetStr: String) =
    cubeSetStr
      .split(",")
      .foldLeft(CubeSet(0, 0, 0)) { (cubeSet, cubes) =>
        if (redPattern.matches(cubes)) {
          val reds = redPattern
            .findAllIn(cubes)
            .matchData
            .map(_.group(1))
            .next()
            .toInt
          CubeSet(reds, cubeSet.green, cubeSet.blue)
        } else if (greenPattern.matches(cubes)) {
          val greens = greenPattern
            .findAllIn(cubes)
            .matchData
            .map(_.group(1))
            .next()
            .toInt
          CubeSet(cubeSet.red, greens, cubeSet.blue)
        } else if (bluePattern.matches(cubes)) {
          val blues = bluePattern
            .findAllIn(cubes)
            .matchData
            .map(_.group(1))
            .next()
            .toInt
          CubeSet(cubeSet.red, cubeSet.green, blues)
        } else cubeSet
      }
}
