package jn.aoc.y2023

import jn.aoc.Utils

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Task_5 {

  case class SourceToDestMap(destStart: Long, sourceRange: Range)

  case class Range(start: Long, end: Long)

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

  def part_2(inputFile: String): Long =
    Using(Source.fromFile(inputFile)) { source =>
      val almanac = parseAlmanac(source.getLines().toList)
      val seedRanges = almanac.seeds.grouped(2).map { pair =>
        val (start, length) = (pair(0), pair(1))
        Range(start, start + length)
      }
      val locations = seedRanges.flatMap { range =>
        val soilRanges = mapSourceToDestRange(range, almanac.seedToSoil)
        val fertilizer = soilRanges.flatMap(mapSourceToDestRange(_, almanac.soilToFertilizer))
        val water = fertilizer.flatMap(mapSourceToDestRange(_, almanac.fertilizerToWater))
        val light = water.flatMap(mapSourceToDestRange(_, almanac.waterToLight))
        val temperature = light.flatMap(mapSourceToDestRange(_, almanac.lightToTemperature))
        val humidity = temperature.flatMap(mapSourceToDestRange(_, almanac.temperatureToHumidity))
        humidity.flatMap(mapSourceToDestRange(_, almanac.humidityToLocation))
      }
      locations.map(_.start).min
    }.get

  private def parseAlmanac(lines: List[String]): Almanac = {
    val descriptions = Utils.splitBy[String](lines, s => s.isBlank)
    val (seedsDescr, mapsDescr) = descriptions.splitAt(1)
    val seeds = seedsDescr.head.head.split(": ")(1).split(" ").map(_.toLong)

    val maps = mapsDescr.map { lines =>
      val (_, data) = lines.splitAt(1)
      data
        .map(s =>
          s.split(" ")
            .map(_.toLong)
        )
        .map(a => SourceToDestMap(a(0), Range(a(1), a(1) + a(2))))
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
    val dest = mapping.filter(isInRange(_, source)).map(calcDest(_, source)).headOption
    dest.getOrElse(source)
  }

  private def isInRange(map: SourceToDestMap, source: Long): Boolean = {
    map.sourceRange.start <= source && source <= map.sourceRange.end
  }

  private def calcDest(map: SourceToDestMap, source: Long): Long = {
    val diff = source - map.sourceRange.start
    map.destStart + diff
  }

  private def mapSourceToDestRange(sourceRange: Range, mapping: Seq[SourceToDestMap]): List[Range] = {
    val destRanges = mapping
      .filter(doOverlap(_, sourceRange))
      .map { map =>
        val sourceRanges = splitIntoRanges(sourceRange, map.sourceRange)
        sourceRanges.map(range => Range(checkRangeAndCalcDest(map, range.start), checkRangeAndCalcDest(map, range.end)))
      }
      .headOption
    destRanges.getOrElse(List(sourceRange))
  }

  private def doOverlap(map: SourceToDestMap, sourceRange: Range): Boolean = {
    !(map.sourceRange.end < sourceRange.start || sourceRange.end < map.sourceRange.start)
  }

  def splitIntoRanges(sourceRange: Range, mapRange: Range): List[Range] = {
    if (mapRange.start < sourceRange.start && sourceRange.start < mapRange.end && mapRange.end < sourceRange.end) {
      List(Range(sourceRange.start, mapRange.end), Range(mapRange.end + 1, sourceRange.end))
    } else if (sourceRange.start < mapRange.start && mapRange.end < sourceRange.end) {
      List(Range(sourceRange.start, mapRange.start - 1), mapRange, Range(mapRange.end + 1, sourceRange.end))
    } else if (
      sourceRange.start < mapRange.start && mapRange.start < sourceRange.end && sourceRange.end < mapRange.end
    ) {
      List(Range(sourceRange.start, mapRange.start - 1), Range(mapRange.start, sourceRange.end))
    } else {
      List(sourceRange)
    }
  }

  private def checkRangeAndCalcDest(map: SourceToDestMap, source: Long): Long = {
    if (isInRange(map, source)) {
      val diff = source - map.sourceRange.start
      map.destStart + diff
    } else source
  }
}
