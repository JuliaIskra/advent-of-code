import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Task_14 {

  def part_1(inputFile: String): Int =
    val sandSource = (500, 0)
    val rock = "rock"
    val sand = "sand"
    Using(Source.fromFile(inputFile)) { source =>
      val rockAndSand = mutable.Map[(Int, Int), String]()

      source
        .getLines()
        .foreach(line =>
          val coordinates = line
            .split(" -> ")
            .map(coordinatePair =>
              val coordinateArray = coordinatePair.split(",")
              (coordinateArray(0).toInt, coordinateArray(1).toInt)
            )
          var coordIdx = 0
          while (coordIdx < coordinates.length - 1) {
            val from = coordinates(coordIdx)
            val to = coordinates(coordIdx + 1)
            if (from._1 == to._1) {
              val step = if (from._2 < to._2) 1 else -1
              (from._2 to to._2 by step).foreach(y => rockAndSand.put((from._1, y), rock))
            } else {
              val step = if (from._1 < to._1) 1 else -1
              (from._1 to to._1 by step).foreach(x => rockAndSand.put((x, from._2), rock))
            }
            coordIdx += 1
          }
        )

      var sandComesToRest = true
      var counter = 0
      while (sandComesToRest) {
        val minX = rockAndSand.keys.map(_._1).min
        val maxX = rockAndSand.keys.map(_._1).max
        val minY = 0
        val maxY = rockAndSand.keys.map(_._2).max

        var (sandX, sandY) = sandSource
        var rested = false
        while (!rested && minX <= sandX && sandX <= maxX && minY <= sandY && sandY <= maxY) {
          if (!rockAndSand.contains((sandX, sandY + 1))) {
            sandY += 1
          } else if (!rockAndSand.contains((sandX - 1, sandY + 1))) {
            sandX -= 1
            sandY += 1
          } else if (!rockAndSand.contains((sandX + 1, sandY + 1))) {
            sandX += 1
            sandY += 1
          } else {
            rested = true
            rockAndSand.put((sandX, sandY), sand)
            counter += 1
          }
        }
        sandComesToRest = rested
      }

      counter
    }.get
}
