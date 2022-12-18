import scala.collection.immutable.Nil
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Using}

@main def main: Unit = {
  println(AdventOfCode.task_11_2("src/main/resources/task_11_input.txt"))
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

  def task_8_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val treesMap = source.getLines
        .map(line => line.map(c => c.toString.toInt))
        .map(_.toList)
        .foldLeft(List[List[Int]]())((list, a) => list.appended(a))

      val maxRow = treesMap.size - 1
      val maxCol = treesMap.head.size - 1
      var invisibleCount = 0

      treesMap.indices
        .foreach(row =>
          treesMap.head.indices
            .foreach(col =>
              // skip edge trees
              if (row != 0 && row != maxRow && col != 0 && col != maxCol) {
                val treeSize = treesMap(row)(col)

                var visibleFromAbove = true
                var aboveRow = row - 1
                while (visibleFromAbove && aboveRow >= 0) {
                  if (treeSize <= treesMap(aboveRow)(col)) {
                    visibleFromAbove = false
                  }
                  aboveRow -= 1
                }

                var visibleFromBelow = true
                var belowRow = row + 1
                while (visibleFromBelow && belowRow <= maxRow) {
                  if (treeSize <= treesMap(belowRow)(col)) {
                    visibleFromBelow = false
                  }
                  belowRow += 1
                }

                var visibleFromLeft = true
                var leftCol = col - 1
                while (visibleFromLeft && leftCol >= 0) {
                  if (treeSize <= treesMap(row)(leftCol)) {
                    visibleFromLeft = false
                  }
                  leftCol -= 1
                }

                var visibleFromRight = true
                var rightCol = col + 1
                while (visibleFromRight && rightCol <= maxCol) {
                  if (treeSize <= treesMap(row)(rightCol)) {
                    visibleFromRight = false
                  }
                  rightCol += 1
                }

                if (
                  !visibleFromAbove && !visibleFromBelow && !visibleFromLeft && !visibleFromRight
                ) {
                  invisibleCount += 1
                }
              }
            )
        )

      treesMap.size * treesMap.head.size - invisibleCount
    }.get

  def task_8_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val treesMap = source.getLines
        .map(line => line.map(c => c.toString.toInt))
        .map(_.toList)
        .foldLeft(List[List[Int]]())((list, a) => list.appended(a))

      val maxRow = treesMap.size - 1
      val maxCol = treesMap.head.size - 1
      var maxScore = 0

      treesMap.indices
        .foreach(row =>
          treesMap.head.indices
            .foreach(col =>
              val treeSize = treesMap(row)(col)
              var stop = false

              var visibleFromAbove = 0
              var aboveRow = row - 1
              while (!stop && aboveRow >= 0) {
                visibleFromAbove += 1
                if (treeSize <= treesMap(aboveRow)(col)) {
                  stop = true
                }
                aboveRow -= 1
              }

              stop = false
              var visibleFromBelow = 0
              var belowRow = row + 1
              while (!stop && belowRow <= maxRow) {
                visibleFromBelow += 1
                if (treeSize <= treesMap(belowRow)(col)) {
                  stop = true
                }
                belowRow += 1
              }

              stop = false
              var visibleFromLeft = 0
              var leftCol = col - 1
              while (!stop && leftCol >= 0) {
                visibleFromLeft += 1
                if (treeSize <= treesMap(row)(leftCol)) {
                  stop = true
                }
                leftCol -= 1
              }

              stop = false
              var visibleFromRight = 0
              var rightCol = col + 1
              while (!stop && rightCol <= maxCol) {
                visibleFromRight += 1
                if (treeSize <= treesMap(row)(rightCol)) {
                  stop = true
                }
                rightCol += 1
              }

              val visibilityScore =
                visibleFromAbove * visibleFromBelow * visibleFromLeft * visibleFromRight
              if (visibilityScore > maxScore) {
                maxScore = visibilityScore
              }
            )
        )

      maxScore
    }.get

  private def task_9_toDiff(move: String): (Int, Int) =
    move match {
      case "L" => (0, -1)
      case "R" => (0, 1)
      case "U" => (1, 0)
      case "D" => (-1, 0)
    }

  private def task_9_newTailPosition(head: (Int, Int), oldTail: (Int, Int)): (Int, Int) = {
    val hX = head._1
    val hY = head._2
    val tX = oldTail._1
    val tY = oldTail._2

    if (
      Math.abs(hX - tX) < 2 && hY == tY || hX == tX && Math.abs(hY - tY) < 2
      || Math.abs(hX - tX) == 1 && Math.abs(hY - tY) == 1
    ) {
      oldTail
    } else if (Math.abs(hX - tX) == 2 && hY == tY) {
      if (hX > tX) {
        (tX + 1, tY)
      } else {
        (tX - 1, tY)
      }
    } else if (hX == tX && Math.abs(hY - tY) == 2) {
      if (hY > tY) {
        (tX, tY + 1)
      } else {
        (tX, tY - 1)
      }
    } else {
      val newTailX = if (hX > tX) tX + 1 else tX - 1
      val newTailY = if (hY > tY) tY + 1 else tY - 1
      (newTailX, newTailY)
    }
  }

  def task_9_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .foldLeft(Set((0, 0)), (0, 0), (0, 0))((state, line) =>
          var (tailVisited, head, tail) = state
          val (move, count) = {
            val split = line.split(" ")
            (split(0), split(1))
          }
          val diff = task_9_toDiff(move)
          (0 until count.toInt)
            .foreach(_ =>
              head = (head._1 + diff._1, head._2 + diff._2)
              tail = task_9_newTailPosition(head, tail)
              tailVisited = tailVisited + tail
            )
          (tailVisited, head, tail)
        )
        ._1
        .size
    }.get

  def task_9_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val rope = Array.fill(9)((0, 0))
      source.getLines
        .foldLeft(Set((0, 0)), (0, 0), rope)((state, line) =>
          var (tailVisited, head, rope) = state
          val (move, count) = {
            val split = line.split(" ")
            (split(0), split(1))
          }
          val diff = task_9_toDiff(move)
          (0 until count.toInt)
            .foreach(_ =>
              head = (head._1 + diff._1, head._2 + diff._2)
              rope = rope
                .foldLeft(Array[(Int, Int)](), head)((state, tail) =>
                  val (rope, prev) = state
                  val newTail = task_9_newTailPosition(prev, tail)
                  (rope :+ newTail, newTail)
                )
                ._1
              tailVisited = tailVisited + rope.last
            )
          (tailVisited, head, rope)
        )
        ._1
        .size
    }.get

  def task_10_1(inputFile: String): Int =
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

  def task_10_2(inputFile: String): String =
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

  private case class Task_11_Monkey(
      var items: Array[Long],
      operation: Long => Long,
      divisibleBy: Int,
      throwToIfTrue: Int,
      throwToIfFalse: Int,
      var inspectedCount: Long = 0L
  )

  private def task_11_parseMonkeys(source: BufferedSource): List[Task_11_Monkey] =
    val itemsPattern = """\s+Starting items: (.*)""".r
    val operationPattern = """\s+Operation: new = (\S+) (\S+) (\S+)""".r
    val divisiblePattern = """\s+Test: divisible by (\d+)""".r
    val truePattern = """\s+If true: throw to monkey (\d+)""".r
    val falsePattern = """\s+If false: throw to monkey (\d+)""".r

    def parseItems(input: String): Array[Long] =
      itemsPattern
        .findAllIn(input)
        .matchData
        .map(_.group(1))
        .next()
        .split(", ")
        .map(_.toLong)

    def parseOperation(input: String): Long => Long =
      operationPattern
        .findAllIn(input)
        .matchData
        .map(m => (m.group(1), m.group(2), m.group(3)))
        .map((o1, op, o2) =>
          val operand1 = o1 match {
            case "old" => None
            case n     => Some(n.toLong)
          }
          val operand2 = o2 match {
            case "old" => None
            case n     => Some(n.toLong)
          }
          op match {
            case "+" =>
              (operand1, operand2) match {
                case (None, None)       => (n: Long) => n + n
                case (None, Some(y))    => (n: Long) => n + y
                case (Some(x), None)    => (n: Long) => x + n
                case (Some(x), Some(y)) => (_: Long) => x + y
              }
            case "-" =>
              (operand1, operand2) match {
                case (None, None)       => (n: Long) => n - n
                case (None, Some(y))    => (n: Long) => n - y
                case (Some(x), None)    => (n: Long) => x - n
                case (Some(x), Some(y)) => (_: Long) => x - y
              }
            case "/" =>
              (operand1, operand2) match {
                case (None, None)       => (n: Long) => n / n
                case (None, Some(y))    => (n: Long) => n / y
                case (Some(x), None)    => (n: Long) => x / n
                case (Some(x), Some(y)) => (_: Long) => x / y
              }
            case "*" =>
              (operand1, operand2) match {
                case (None, None)       => (n: Long) => n * n
                case (None, Some(y))    => (n: Long) => n * y
                case (Some(x), None)    => (n: Long) => x * n
                case (Some(x), Some(y)) => (_: Long) => x * y
              }
          }
        )
        .next()

    source.getLines
      .filterNot(_.isEmpty)
      .grouped(6)
      .foldLeft(List[Task_11_Monkey]())((monkeys, lines) =>
        var items = Array[Long]()
        var operation = (n: Long) => n
        var divisibleBy = 0
        var ifTrue = 0
        var ifFalse = 0

        lines.foreach(line =>
          if (itemsPattern.matches(line)) {
            items = parseItems(line)
          }
          if (operationPattern.matches(line)) {
            operation = parseOperation(line)
          }
          if (divisiblePattern.matches(line)) {
            divisibleBy = divisiblePattern.findAllIn(line).matchData.map(_.group(1).toInt).next()
          }
          if (truePattern.matches(line)) {
            ifTrue = truePattern.findAllIn(line).matchData.map(_.group(1).toInt).next()
          }
          if (falsePattern.matches(line)) {
            ifFalse = falsePattern.findAllIn(line).matchData.map(_.group(1).toInt).next()
          }
        )
        monkeys :+ Task_11_Monkey(items, operation, divisibleBy, ifTrue, ifFalse)
      )

  private def task_11_playRounds(
      monkeys: List[Task_11_Monkey],
      rounds: Int,
      worryChangeFunction: Long => Long
  ): Unit =
    var round = 0
    while (round < rounds) {
      monkeys.foreach(monkey =>
        monkey.items.foreach(worry =>
          val newWorry = worryChangeFunction.apply(monkey.operation.apply(worry))
          val nextMonkeyIdx =
            if (newWorry % monkey.divisibleBy == 0) monkey.throwToIfTrue
            else monkey.throwToIfFalse
          val nextMonkey = monkeys(nextMonkeyIdx)
          nextMonkey.items = nextMonkey.items.appended(newWorry)
          monkey.inspectedCount += 1
        )
        monkey.items = Array[Long]()
      )
      round += 1
    }

  def task_11_1(inputFile: String): Long =
    val rounds = 20

    Using(Source.fromFile(inputFile)) { source =>
      val monkeys = task_11_parseMonkeys(source)
      task_11_playRounds(monkeys, rounds, worry => worry / 3)
      monkeys.map(_.inspectedCount).sorted.takeRight(2).product
    }.get

  def task_11_2(inputFile: String): Long =
    val rounds = 10000

    Using(Source.fromFile(inputFile)) { source =>
      val monkeys = task_11_parseMonkeys(source)
      val commonMultiple = monkeys.map(_.divisibleBy).product
      task_11_playRounds(monkeys, rounds, worry => worry % commonMultiple)
      monkeys.map(_.inspectedCount).sorted.takeRight(2).product
    }.get
}
