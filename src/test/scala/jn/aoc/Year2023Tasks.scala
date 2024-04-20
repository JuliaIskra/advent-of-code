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
}
