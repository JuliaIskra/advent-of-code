package jn.aoc

import scala.annotation.tailrec

object Utils {

  def partitionBy[T](col: Seq[T], pred: T => Boolean): List[Seq[T]] = {

    @tailrec
    def partitionByRecur(col: Seq[T], acc: List[Seq[T]]): List[Seq[T]] = {
      val i = col.indexWhere(pred)
      if (i > -1) {
        val (part, rest) = (col.take(i), col.drop(i + 1))
        partitionByRecur(rest, acc :+ part)
      } else {
        acc :+ col
      }
    }

    partitionByRecur(col, List())
  }
}
