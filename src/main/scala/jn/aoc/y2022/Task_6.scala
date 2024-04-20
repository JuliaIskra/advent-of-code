package jn.aoc.y2022

import scala.io.Source
import scala.util.Using

object Task_6 {

  private def getMarkerEnd(inputFile: String, markerSize: Int): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .next()
        .foldLeft(("", 0))((state, c) => {
          val (marker, i) = state
          if (marker.length < markerSize) {
            (marker + c, i + 1)
          } else {
            if (marker.toSet.size == markerSize) {
              (marker, i)
            } else {
              (marker.substring(1) + c, i + 1)
            }
          }
        })
        ._2
    }.get

  def part_1(inputFile: String): Int =
    getMarkerEnd(inputFile, 4)

  def part_2(inputFile: String): Int =
    getMarkerEnd(inputFile, 14)
}
