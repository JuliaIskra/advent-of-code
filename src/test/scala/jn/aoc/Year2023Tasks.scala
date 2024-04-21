package jn.aoc

import jn.aoc.y2023.*

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Year2023Tasks extends munit.FunSuite {
  test("task 1.1 example") {
    val inputFile = "src/test/resources/2023/task_01_example.txt"
    val result = Task_1.part_1(inputFile)
    assertEquals(result, 142)
  }

  test("task 1.2 example") {
    val inputFile = "src/test/resources/2023/task_01_example_1.txt"
    val result = Task_1.part_2(inputFile)
    assertEquals(result, 335)
  }

  test("task 2.1 example") {
    val inputFile = "src/test/resources/2023/task_02_example.txt"
    val result = Task_2.part_1(inputFile, 12, 13, 14)
    assertEquals(result, 8)
  }

  test("task 2.2 example") {
    val inputFile = "src/test/resources/2023/task_02_example.txt"
    val result = Task_2.part_2(inputFile)
    assertEquals(result, 2286)
  }

  test("task 3.1 example") {
    val inputFile = "src/test/resources/2023/task_03_example.txt"
    val result = Task_3.part_1(inputFile)
    assertEquals(result, 4361)
  }

  test("task 3.2 example") {
    val inputFile = "src/test/resources/2023/task_03_example.txt"
    val result = Task_3.part_2(inputFile)
    assertEquals(result, 467835)
  }

  test("task 4.1 example") {
    val inputFile = "src/test/resources/2023/task_04_example.txt"
    val result = Task_4.part_1(inputFile)
    assertEquals(result, 13)
  }

  test("task 4.2 example") {
    val inputFile = "src/test/resources/2023/task_04_example.txt"
    val result = Task_4.part_2(inputFile)
    assertEquals(result, 30)
  }

  test("task 5.1 example") {
    val inputFile = "src/test/resources/2023/task_05_example.txt"
    val result = Task_5.part_1(inputFile)
    assertEquals(result, 35L)
  }

  test("task 5.2 example") {
    val inputFile = "src/test/resources/2023/task_05_example.txt"
    val result = Task_5.part_2(inputFile)
    assertEquals(result, 46L)
  }

  test("splitIntoRanges when map range is before source range") {
    import Task_5.Range

    val ranges = Task_5.splitIntoRanges(Range(30, 40), Range(10, 20))
    assertEquals(ranges, List(Range(30, 40)))
  }

  test("splitIntoRanges when map range overlaps with source range over start") {
    import Task_5.Range

    val ranges = Task_5.splitIntoRanges(Range(20, 40), Range(10, 30))
    assertEquals(ranges, List(Range(20, 30), Range(31, 40)))
  }

  test("splitIntoRanges when map range includes source range") {
    import Task_5.Range

    val ranges = Task_5.splitIntoRanges(Range(20, 30), Range(10, 40))
    assertEquals(ranges, List(Range(20, 30)))
  }

  test("splitIntoRanges when source range includes map range") {
    import Task_5.Range

    val ranges = Task_5.splitIntoRanges(Range(10, 40), Range(20, 30))
    assertEquals(ranges, List(Range(10, 19), Range(20, 30), Range(31, 40)))
  }

  test("splitIntoRanges when map range overlaps with source range over end") {
    import Task_5.Range

    val ranges = Task_5.splitIntoRanges(Range(10, 30), Range(20, 40))
    assertEquals(ranges, List(Range(10, 19), Range(20, 30)))
  }

  test("splitIntoRanges when source range is before map range") {
    import Task_5.Range

    val ranges = Task_5.splitIntoRanges(Range(10, 20), Range(30, 40))
    assertEquals(ranges, List(Range(10, 20)))
  }
}
