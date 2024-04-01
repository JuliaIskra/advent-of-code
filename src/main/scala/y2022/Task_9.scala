package y2022

import scala.io.Source
import scala.util.Using

object Task_9 {

  private def toDiff(move: String): (Int, Int) =
    move match {
      case "L" => (0, -1)
      case "R" => (0, 1)
      case "U" => (1, 0)
      case "D" => (-1, 0)
    }

  private def newTailPosition(head: (Int, Int), oldTail: (Int, Int)): (Int, Int) = {
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

  def part_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .foldLeft(Set((0, 0)), (0, 0), (0, 0))((state, line) =>
          var (tailVisited, head, tail) = state
          val (move, count) = {
            val split = line.split(" ")
            (split(0), split(1))
          }
          val diff = toDiff(move)
          (0 until count.toInt)
            .foreach(_ =>
              head = (head._1 + diff._1, head._2 + diff._2)
              tail = newTailPosition(head, tail)
              tailVisited = tailVisited + tail
            )
          (tailVisited, head, tail)
        )
        ._1
        .size
    }.get

  def part_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val rope = Array.fill(9)((0, 0))
      source.getLines
        .foldLeft(Set((0, 0)), (0, 0), rope)((state, line) =>
          var (tailVisited, head, rope) = state
          val (move, count) = {
            val split = line.split(" ")
            (split(0), split(1))
          }
          val diff = toDiff(move)
          (0 until count.toInt)
            .foreach(_ =>
              head = (head._1 + diff._1, head._2 + diff._2)
              rope = rope
                .foldLeft(Array[(Int, Int)](), head)((state, tail) =>
                  val (rope, prev) = state
                  val newTail = newTailPosition(prev, tail)
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
}
