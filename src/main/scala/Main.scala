import scala.io.Source
import scala.util.{Failure, Success, Using}

@main def main: Unit = {
  println(AdventOfCode.task_2_2("src/main/resources/task_2_input.txt"))
}

object AdventOfCode {

  def task_1_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val cals = source.getLines.toList
      var max = 0
      var currentSum = 0
      var i = 0
      while (i < cals.size) {
        cals(i) match {
          case "" =>
            if (currentSum > max) {
              max = currentSum
            }
            currentSum = 0
          case cal =>
            currentSum = currentSum + cal.toInt
        }
        i = i + 1
      }
      if (currentSum > max) {
        max = currentSum
      }
      max
    } match {
      case Success(n) => n
      case Failure(e) => throw new RuntimeException(e)
    }

  def task_1_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val cals = source.getLines.toList
      val elvesCount = cals.partition(_.equals(""))._1.size + 1
      val elves = new Array[Int](elvesCount)
      var currentSum = 0
      var c = 0
      var e = 0
      while (c < cals.size) {
        cals(c) match {
          case "" =>
            elves(e) = currentSum
            e = e + 1
            currentSum = 0
          case cal =>
            currentSum = currentSum + cal.toInt
        }
        c = c + 1
      }
      elves(e) = currentSum
      elves.sorted.takeRight(3).sum
    } match {
      case Success(n) => n
      case Failure(e) => throw new RuntimeException(e)
    }

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
      source.getLines.toList
        .map(_.split(" "))
        .map(codes => (codes(0), codes(1)))
        .map((a, b) => shapeScore(b) + roundScore((a, b)))
        .sum
    } match {
      case Success(n) => n
      case Failure(e) => throw new RuntimeException(e)
    }
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
      source.getLines.toList
        .map(_.split(" "))
        .map(codes => getScore((codes(0), codes(1))))
        .sum
    } match {
      case Success(n) => n
      case Failure(e) => throw new RuntimeException(e)
    }
  }
}
