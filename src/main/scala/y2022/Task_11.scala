package y2022

import scala.io.Source
import scala.util.Using

object Task_11 {

  private case class Monkey(
      var items: Array[Long],
      operation: Long => Long,
      divisibleBy: Int,
      throwToIfTrue: Int,
      throwToIfFalse: Int,
      var inspectedCount: Long = 0L
  )

  private def parseMonkeys(inputFile: String): List[Monkey] =
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

    Using(Source.fromFile(inputFile)) { source =>
      source.getLines
        .filterNot(_.isEmpty)
        .grouped(6)
        .foldLeft(List[Monkey]())((monkeys, lines) =>
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
          monkeys :+ Monkey(items, operation, divisibleBy, ifTrue, ifFalse)
        )
    }.get

  private def playRounds(
      monkeys: List[Monkey],
      rounds: Int,
      worryChangeFunction: Long => Long
  ): Unit =
    var round = 0
    while (round < rounds) {
      monkeys.foreach(monkey =>
        monkey.items.foreach(worry =>
          val newWorry = worryChangeFunction(monkey.operation(worry))
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

  def part_1(inputFile: String): Long =
    val rounds = 20
    val monkeys = parseMonkeys(inputFile)
    playRounds(monkeys, rounds, worry => worry / 3)
    monkeys.map(_.inspectedCount).sorted.takeRight(2).product

  def part_2(inputFile: String): Long =
    val rounds = 10000
    val monkeys = parseMonkeys(inputFile)
    val commonMultiple = monkeys.map(_.divisibleBy).product
    playRounds(monkeys, rounds, worry => worry % commonMultiple)
    monkeys.map(_.inspectedCount).sorted.takeRight(2).product
}
