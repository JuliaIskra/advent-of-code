package y2022

import scala.io.Source
import scala.util.Using

object Task_10 {

  def part_1(inputFile: String): Int =
    val cyclesToCount = (20 to 220 by 40).toSet
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .foldLeft((1, 1, 0))((state, line) =>
          val (cycle, x, sum) = state
          line match {
            case "noop" =>
              val strength =
                if (cyclesToCount.contains(cycle)) cycle * x
                else 0
              (cycle + 1, x, sum + strength)
            case line =>
              val diff = line.split(" ")(1).toInt
              val strength =
                if (cyclesToCount.contains(cycle)) cycle * x
                else if (cyclesToCount.contains(cycle + 1)) (cycle + 1) * x
                else 0
              (cycle + 2, x + diff, sum + strength)
          }
        )
        ._3
    }.get

  def part_2(inputFile: String): String =
    val crtWidth = 40
    val crtHeight = 6
    val litPixel = "#"
    val darkPixel = "."

    def getCurrentPixel(crtRow: Int, spriteMiddle: Int): String =
      val crtAndSpriteIntersect = spriteMiddle - 1 <= crtRow && crtRow <= spriteMiddle + 1
      if (crtAndSpriteIntersect) litPixel else darkPixel

    def updateCrtAccordingToCycle(cycle: Int, crt: Array[String], x: Int): Array[String] =
      val crtRow = (cycle - 1) / crtWidth
      val crtCol = (cycle - 1) % crtWidth
      val pxl = getCurrentPixel(crtCol, x)
      crt.updated(crtRow, crt(crtRow) + pxl)

    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .foldLeft((1, 1, Array.fill(crtHeight)("")))((state, line) =>
          val (cycle, x, crt) = state
          line match {
            case "noop" =>
              (cycle + 1, x, updateCrtAccordingToCycle(cycle, crt, x))
            case line =>
              val currentCycleCrt = updateCrtAccordingToCycle(cycle, crt, x)
              val nextCycleCrt = updateCrtAccordingToCycle(cycle + 1, currentCycleCrt, x)
              val diff = line.split(" ")(1).toInt
              (cycle + 2, x + diff, nextCycleCrt)
          }
        )
        ._3
        .reduce(_ + "\n" + _)
    }.get
}
