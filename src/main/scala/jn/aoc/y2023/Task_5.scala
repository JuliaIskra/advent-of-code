package jn.aoc.y2023

import jn.aoc.Utils

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Task_5 {

  case class SourceToDestMap(destStart: Long, sourceStart: Long, length: Long) {
    def isInRange(source: Long): Boolean = {
      sourceStart <= source && source <= sourceStart + length
    }

    def getDest(source: Long): Long = {
      val diff = source - sourceStart
      destStart + diff
    }
  }

  case class Almanac(
      seeds: Seq[Long],
      seedToSoil: Seq[SourceToDestMap],
      soilToFertilizer: Seq[SourceToDestMap],
      fertilizerToWater: Seq[SourceToDestMap],
      waterToLight: Seq[SourceToDestMap],
      lightToTemperature: Seq[SourceToDestMap],
      temperatureToHumidity: Seq[SourceToDestMap],
      humidityToLocation: Seq[SourceToDestMap]
  )

  def part_1(inputFile: String): Long =
    Using(Source.fromFile(inputFile)) { source =>
      val almanac = parseAlmanac(source.getLines().toList)
      val locations = almanac.seeds.map { seed =>
        val soil = mapSourceToDest(seed, almanac.seedToSoil)
        val fertilizer = mapSourceToDest(soil, almanac.soilToFertilizer)
        val water = mapSourceToDest(fertilizer, almanac.fertilizerToWater)
        val light = mapSourceToDest(water, almanac.waterToLight)
        val temperature = mapSourceToDest(light, almanac.lightToTemperature)
        val humidity = mapSourceToDest(temperature, almanac.temperatureToHumidity)
        mapSourceToDest(humidity, almanac.humidityToLocation)
      }
      locations.min
    }.get

  private def parseAlmanac(lines: List[String]): Almanac = {
    val partitionedLines = Utils.partitionBy[String](lines, s => s.isBlank)
    val (seedsDescr, mapsDescr) = partitionedLines.splitAt(1)
    val seeds = seedsDescr.head.head.split(": ")(1).split(" ").map(_.toLong)

    val maps = mapsDescr.map { lines =>
      val (_, data) = lines.splitAt(1)
      data
        .map(s =>
          s.split(" ")
            .map(_.toLong)
        )
        .map(a => SourceToDestMap(a(0), a(1), a(2)))
    }

    Almanac(
      seeds,
      seedToSoil = maps(0),
      soilToFertilizer = maps(1),
      fertilizerToWater = maps(2),
      waterToLight = maps(3),
      lightToTemperature = maps(4),
      temperatureToHumidity = maps(5),
      humidityToLocation = maps(6)
    )
  }

  private def mapSourceToDest(source: Long, mapping: Seq[SourceToDestMap]): Long = {
    val dest = mapping.filter(_.isInRange(source)).map(_.getDest(source)).headOption
    dest.getOrElse(source)
  }
}
