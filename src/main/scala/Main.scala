import scala.io.Source
import scala.util.{Failure, Success, Using}

@main def main: Unit = {
  println("Task 1.1: " + AdventOfCode.task_1_1("src/main/resources/task_1_input.txt"))
  println("Task 1.2: " + AdventOfCode.task_1_2("src/main/resources/task_1_input.txt"))
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
}
