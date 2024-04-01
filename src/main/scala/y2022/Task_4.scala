package y2022

import scala.io.Source
import scala.util.Using

object Task_4 {

  private def countSectionsIf(inputFile: String, p: Array[Int] => Boolean): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .map(line =>
          for {
            range <- line.split(",")
            n <- range.split("-")
          } yield n.toInt
        )
        .count(sections => p(sections))
    }.get

  def part_1(inputFile: String): Int =
    countSectionsIf(
      inputFile,
      sections => {
        val start1 = sections(0)
        val end1 = sections(1)
        val start2 = sections(2)
        val end2 = sections(3)
        // s1.........e1
        //    s2...e2
        // or
        //    s1...e1
        // s2.........e2
        start1 >= start2 && end1 <= end2 || start2 >= start1 && end2 <= end1
      }
    )

  def part_2(inputFile: String): Int =
    countSectionsIf(
      inputFile,
      sections => {
        val start1 = sections(0)
        val end1 = sections(1)
        val start2 = sections(2)
        val end2 = sections(3)
        // overlap
        start1 <= start2 && start2 <= end1
        || start2 <= start1 && start1 <= end2
        || start2 <= end1 && end1 <= end2
        || start1 <= end2 && end2 <= end1
      }
    )
}
