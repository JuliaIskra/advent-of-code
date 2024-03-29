package y2022

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Task_5 {

  private def rearrangeStacks(
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

  def part_1(inputFile: String): String =
    rearrangeStacks(
      inputFile,
      (stacks, nToMove, from, to) =>
        (0 until nToMove)
          .foreach(_ => {
            val toMove = stacks(from - 1).pop()
            stacks(to - 1).push(toMove)
          })
    )

  def part_2(inputFile: String): String =
    rearrangeStacks(
      inputFile,
      (stacks, nToMove, from, to) =>
        val toMove = new Array[String](nToMove)
        (0 until nToMove)
          .foreach(i => toMove(i) = stacks(from - 1).pop())
        (nToMove until 0 by -1)
          .foreach(i => stacks(to - 1).push(toMove(i - 1)))
    )
}
