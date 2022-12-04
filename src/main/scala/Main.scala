import scala.collection.immutable.Nil
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{Failure, Success, Using}

@main def main: Unit = {
  println(AdventOfCode.task_3_2("src/main/resources/task_3_input.txt"))
}

object AdventOfCode {

  def task_1_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      var max = 0
      var currentSum = 0
      source.getLines
        .foreach {
          case "" =>
            if (currentSum > max) {
              max = currentSum
            }
            currentSum = 0
          case cal =>
            currentSum = currentSum + cal.toInt
        }
      if (currentSum > max) {
        max = currentSum
      }
      max
    }.get

  def task_1_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .foldLeft(0 :: Nil)((list, line) => {
          val head :: rest = list: @unchecked
          line match {
            case "" =>
              0 :: head :: rest
            case n =>
              head + n.toInt :: rest
          }
        })
        .sorted
        .takeRight(3)
        .sum
    }.get

  def task_2_1(inputFile: String): Int = {

    def shapeScore(code: String): Int =
      code match {
        case "X" => 1
        case "Y" => 2
        case "Z" => 3
      }

    def roundScore(combination: (String, String)): Int =
      combination match {
        case ("A", "X") | ("B", "Y") | ("C", "Z") => 3
        case ("A", "Y") | ("B", "Z") | ("C", "X") => 6
        case ("A", "Z") | ("B", "X") | ("C", "Y") => 0
      }

    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .map(_.split(" "))
        .map(codes => (codes(0), codes(1)))
        .map((a, b) => shapeScore(b) + roundScore((a, b)))
        .sum
    }.get
  }

  def task_2_2(inputFile: String): Int = {

    def getScore(codes: (String, String)): Int =
      codes match {
        case ("A", "X") => 3 + 0
        case ("B", "X") => 1 + 0
        case ("C", "X") => 2 + 0
        case ("A", "Y") => 1 + 3
        case ("B", "Y") => 2 + 3
        case ("C", "Y") => 3 + 3
        case ("A", "Z") => 2 + 6
        case ("B", "Z") => 3 + 6
        case ("C", "Z") => 1 + 6
      }

    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .map(_.split(" "))
        .map(codes => getScore((codes(0), codes(1))))
        .sum
    }.get
  }

  def task_3_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .map(items => items.splitAt(items.length / 2))
        .map((a, b) => (a.toSet, b.toSet))
        .map((a, b) => a.intersect(b))
        .map(_.head)
        .map(c =>
          if (c >= 'a') c - 'a' + 1
          else c - 'A' + 27
        )
        .sum
    }.get

  def task_3_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .foldLeft((List[List[String]](), 1))((state, line) => {
          val (list, counter) = state
          if (counter % 3 == 1) {
            (List[String](line) :: list, counter + 1)
          } else {
            val head :: rest = list: @unchecked
            ((line :: head) :: rest, counter + 1)
          }
        })
        ._1
        .map(_.map(_.toSet).reduce(_.intersect(_)))
        .map(_.head)
        .map(c =>
          if (c >= 'a') c - 'a' + 1
          else c - 'A' + 27
        )
        .sum
    }.get
}
