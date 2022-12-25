// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("task 1.1 example") {
    val inputFile = "src/test/resources/task_01_example.txt"
    val result = AdventOfCode.task_1_1(inputFile)
    assertEquals(result, 24000)
  }

  test("task 1.2 example") {
    val inputFile = "src/test/resources/task_01_example.txt"
    val result = AdventOfCode.task_1_2(inputFile)
    assertEquals(result, 45000)
  }

  test("task 2.1 example") {
    val inputFile = "src/test/resources/task_02_example.txt"
    val result = AdventOfCode.task_2_1(inputFile)
    assertEquals(result, 15)
  }

  test("task 2.2 example") {
    val inputFile = "src/test/resources/task_02_example.txt"
    val result = AdventOfCode.task_2_2(inputFile)
    assertEquals(result, 12)
  }

  test("task 3.1 example") {
    val inputFile = "src/test/resources/task_03_example.txt"
    val result = AdventOfCode.task_3_1(inputFile)
    assertEquals(result, 157)
  }

  test("task 3.2 example") {
    val inputFile = "src/test/resources/task_03_example.txt"
    val result = AdventOfCode.task_3_2(inputFile)
    assertEquals(result, 70)
  }

  test("task 4.1 example") {
    val inputFile = "src/test/resources/task_04_example.txt"
    val result = AdventOfCode.task_4_1(inputFile)
    assertEquals(result, 2)
  }

  test("task 4.2 example") {
    val inputFile = "src/test/resources/task_04_example.txt"
    val result = AdventOfCode.task_4_2(inputFile)
    assertEquals(result, 4)
  }

  test("task 5.1 example") {
    val inputFile = "src/test/resources/task_05_example.txt"
    val result = AdventOfCode.task_5_1(inputFile)
    assertEquals(result, "CMZ")
  }

  test("task 5.2 example") {
    val inputFile = "src/test/resources/task_05_example.txt"
    val result = AdventOfCode.task_5_2(inputFile)
    assertEquals(result, "MCD")
  }

  test("task 6.1 example 1") {
    val inputFile = "src/test/resources/task_06_example_1.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 7)
  }

  test("task 6.1 example 2") {
    val inputFile = "src/test/resources/task_06_example_2.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 5)
  }

  test("task 6.1 example 3") {
    val inputFile = "src/test/resources/task_06_example_3.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 6)
  }

  test("task 6.1 example 4") {
    val inputFile = "src/test/resources/task_06_example_4.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 10)
  }

  test("task 6.1 example 5") {
    val inputFile = "src/test/resources/task_06_example_5.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 11)
  }

  test("task 6.2 example 1") {
    val inputFile = "src/test/resources/task_06_example_1.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 19)
  }

  test("task 6.2 example 2") {
    val inputFile = "src/test/resources/task_06_example_2.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 23)
  }

  test("task 6.2 example 3") {
    val inputFile = "src/test/resources/task_06_example_3.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 23)
  }

  test("task 6.2 example 4") {
    val inputFile = "src/test/resources/task_06_example_4.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 29)
  }

  test("task 6.2 example 5") {
    val inputFile = "src/test/resources/task_06_example_5.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 26)
  }

  test("task 7.1 example") {
    val inputFile = "src/test/resources/task_07_example.txt"
    val result = AdventOfCode.task_7_1(inputFile)
    assertEquals(result, 95437)
  }

  test("task 7.2 example") {
    val inputFile = "src/test/resources/task_07_example.txt"
    val result = AdventOfCode.task_7_2(inputFile)
    assertEquals(result, 24933642)
  }

  test("task 8.1 example") {
    val inputFile = "src/test/resources/task_08_example.txt"
    val result = AdventOfCode.task_8_1(inputFile)
    assertEquals(result, 21)
  }

  test("task 8.2 example") {
    val inputFile = "src/test/resources/task_08_example.txt"
    val result = AdventOfCode.task_8_2(inputFile)
    assertEquals(result, 8)
  }

  test("task 9.1 example") {
    val inputFile = "src/test/resources/task_09_example_1.txt"
    val result = AdventOfCode.task_9_1(inputFile)
    assertEquals(result, 13)
  }

  test("task 9.2 example 1") {
    val inputFile = "src/test/resources/task_09_example_1.txt"
    val result = AdventOfCode.task_9_2(inputFile)
    assertEquals(result, 1)
  }

  test("task 9.2 example 2") {
    val inputFile = "src/test/resources/task_09_example_2.txt"
    val result = AdventOfCode.task_9_2(inputFile)
    assertEquals(result, 36)
  }

  test("task 10.1 example") {
    val inputFile = "src/test/resources/task_10_example.txt"
    val result = AdventOfCode.task_10_1(inputFile)
    assertEquals(result, 13140)
  }

  test("task 10.2 example") {
    val inputFile = "src/test/resources/task_10_example.txt"
    val result = AdventOfCode.task_10_2(inputFile)
    val expected =
      """##..##..##..##..##..##..##..##..##..##..
        |###...###...###...###...###...###...###.
        |####....####....####....####....####....
        |#####.....#####.....#####.....#####.....
        |######......######......######......####
        |#######.......#######.......#######.....""".stripMargin
    assertEquals(result, expected)
  }

  test("task 11.1 example") {
    val inputFile = "src/test/resources/task_11_example.txt"
    val result = AdventOfCode.task_11_1(inputFile)
    assertEquals(result, 10605L)
  }

  test("task 11.2 example") {
    val inputFile = "src/test/resources/task_11_example.txt"
    val result = AdventOfCode.task_11_2(inputFile)
    assertEquals(result, 2713310158L)
  }

  test("task 12.1 example") {
    val inputFile = "src/test/resources/task_12_example.txt"
    val result = AdventOfCode.task_12_1(inputFile)
    assertEquals(result, 31)
  }

  test("task 12.2 example") {
    val inputFile = "src/test/resources/task_12_example.txt"
    val result = AdventOfCode.task_12_2(inputFile)
    assertEquals(result, 29)
  }

  test("task 13.1 example") {
    val inputFile = "src/test/resources/task_13_example.txt"
    val result = AdventOfCode.task_13_1(inputFile)
    assertEquals(result, 13)
  }
}
