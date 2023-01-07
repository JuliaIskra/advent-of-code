import scala.io.Source
import scala.util.Using

object Task_15 {

  private def parseSensorsInput(inputFile: String): Set[((Int, Int), (Int, Int))] =
    val pattern = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r

    Using(Source.fromFile(inputFile)) { source =>
      source
        .getLines()
        .foldLeft(Set[((Int, Int), (Int, Int))]())((coveredCells, line) =>
          val coordinates = pattern
            .findAllIn(line)
            .matchData
            .map(m => ((m.group(1).toInt, m.group(2).toInt), (m.group(3).toInt, m.group(4).toInt)))
            .next()
          coveredCells + coordinates
        )
    }.get

  def part_1(inputFile: String, y: Int): Int =
    val sensorBeaconCoordinates = parseSensorsInput(inputFile)

    sensorBeaconCoordinates
      .foldLeft((Set[Int](), Set[Int]()))((state, sensorBeacon) =>
        val (nonBeaconPositions, beaconPositions) = state
        val (sensor, beacon) = sensorBeacon
        val distance = Math.abs(sensor._1 - beacon._1) + Math.abs(sensor._2 - beacon._2)
        val top = sensor._2 - distance
        val bottom = sensor._2 + distance
        if (top <= y && y <= bottom) {
          val distanceToY = Math.abs(sensor._2 - y)
          val left = sensor._1 - (distance - distanceToY)
          val right = sensor._1 + (distance - distanceToY)
          val updatedBeaconPositions =
            if (beacon._2 == y) {
              beaconPositions + beacon._1
            } else {
              beaconPositions
            }
          val nonBeacons = (left to right).toSet.diff(updatedBeaconPositions)
          (nonBeaconPositions ++ nonBeacons, updatedBeaconPositions)
        } else {
          (nonBeaconPositions, beaconPositions)
        }
      )._1.size
}
