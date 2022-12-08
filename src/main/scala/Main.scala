import scala.collection.immutable.Nil
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{Failure, Success, Using}
import scala.collection.mutable.Stack

@main def main: Unit = {
  println(AdventOfCode.task_7_2("src/main/resources/task_7_input.txt"))
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

  private def task_4_countSectionsIf(inputFile: String, p: Array[Int] => Boolean): Int =
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

  def task_4_1(inputFile: String): Int =
    task_4_countSectionsIf(
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

  def task_4_2(inputFile: String): Int =
    task_4_countSectionsIf(
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

  private def task_5_rearrangeStacks(
      inputFile: String,
      how: (List[mutable.Stack[String]], Int, Int, Int) => Unit
  ): String =
    Using(Source.fromFile(inputFile)) { source =>
      val lines = source.getLines.toList
      val separator = lines.indexOf("")
      val (stacksInput, actions) = (lines.take(separator), lines.drop(separator + 1))

      val stacks: List[mutable.Stack[String]] = stacksInput
        .foldRight(List[mutable.Stack[String]]())((line, stacks) => {
          if (stacks.isEmpty) {
            val stacksNumber = line.split("  ").map(_.trim).length
            (0 until stacksNumber).map(_ => mutable.Stack[String]()).toList
          } else {
            line
              .foldLeft((stacks, 0))((state, c) => {
                val (stacks, i) = state
                if ('A' <= c && c <= 'Z') {
                  val stackNumber = (i + 3) / 4
                  stacks(stackNumber - 1).push(c.toString)
                }
                (stacks, i + 1)
              })
              ._1
          }
        })

      actions.foreach(line => {
        val pattern = """move (\d+) from (\d+) to (\d+)""".r
        val (nToMove, from, to) = pattern
          .findAllIn(line)
          .matchData
          .map(m => (m.group(1).toInt, m.group(2).toInt, m.group(3).toInt))
          .toList
          .head
        how(stacks, nToMove, from, to)
      })

      stacks.map(_.pop()).reduce(_ + _)
    }.get

  def task_5_1(inputFile: String): String =
    task_5_rearrangeStacks(
      inputFile,
      (stacks, nToMove, from, to) =>
        (0 until nToMove)
          .foreach(_ => {
            val toMove = stacks(from - 1).pop()
            stacks(to - 1).push(toMove)
          })
    )

  def task_5_2(inputFile: String): String =
    task_5_rearrangeStacks(
      inputFile,
      (stacks, nToMove, from, to) =>
        val toMove = new Array[String](nToMove)
        (0 until nToMove)
          .foreach(i => toMove(i) = stacks(from - 1).pop())
        (nToMove until 0 by -1)
          .foreach(i => stacks(to - 1).push(toMove(i - 1)))
    )

  private def task_6_getMarkerEnd(inputFile: String, markerSize: Int): Int =
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

  def task_6_1(inputFile: String): Int =
    task_6_getMarkerEnd(inputFile, 4)

  def task_6_2(inputFile: String): Int =
    task_6_getMarkerEnd(inputFile, 14)

  private def task_7_parseDirs(inputFile: String): mutable.Map[String, Int] =
    Using(Source.fromFile(inputFile)) { source =>
      val dirs = mutable.Map[String, Int]()
      var path = Array[String]()
      source.getLines
        .foreach(line => {
          if (line.startsWith("$")) {
            line match {
              case "$ ls" => // do nothing
              case cdDir =>
                cdDir.substring(5) match {
                  case "/" =>
                    dirs.put("root", 0)
                    path = Array("root")
                  case ".." =>
                    path = path.take(path.length - 1)
                  case dir =>
                    val fullDirName = path.reduce(_ + "/" + _) + "/" + dir
                    dirs.put(fullDirName, 0)
                    path = path.appended(dir)
                }
            }
          } else {
            val split = line.split(" ")
            (split(0), split(1)) match {
              case ("dir", dir) =>
                val fullDirName = path.reduce(_ + "/" + _) + "/" + dir
                dirs.put(fullDirName, 0)
              case (size, _) =>
                var fullDirName = ""
                path
                  .foreach(dir => {
                    fullDirName = if (fullDirName == "") dir else fullDirName + "/" + dir
                    val currentSize = dirs(fullDirName)
                    dirs.put(fullDirName, currentSize + size.toInt)
                  })
            }
          }
        })
      dirs
    }.get

  def task_7_1(inputFile: String): Int =
    val dirs = task_7_parseDirs(inputFile)
    dirs.filter((_, size) => size <= 100000).values.sum

  def task_7_2(inputFile: String): Int =
    val totalSpace = 70000000
    val neededSpace = 30000000

    val dirs = task_7_parseDirs(inputFile)

    val usedSpace = dirs("root")
    val spaceToFree = neededSpace - (totalSpace - usedSpace)

    dirs.values.foldLeft(usedSpace)((bestToRemove, dirSize) =>
      if (dirSize > spaceToFree && dirSize < bestToRemove) {
        dirSize
      } else {
        bestToRemove
      }
    )
}
