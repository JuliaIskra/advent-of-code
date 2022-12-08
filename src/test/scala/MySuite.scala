// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("task 1.1 example") {
    val inputFile = "src/test/resources/task_1_example.txt"
    val result = AdventOfCode.task_1_1(inputFile)
    assertEquals(result, 24000)
  }

  test("task 1.2 example") {
    val inputFile = "src/test/resources/task_1_example.txt"
    val result = AdventOfCode.task_1_2(inputFile)
    assertEquals(result, 45000)
  }

  test("task 2.1 example") {
    val inputFile = "src/test/resources/task_2_example.txt"
    val result = AdventOfCode.task_2_1(inputFile)
    assertEquals(result, 15)
  }

  test("task 2.2 example") {
    val inputFile = "src/test/resources/task_2_example.txt"
    val result = AdventOfCode.task_2_2(inputFile)
    assertEquals(result, 12)
  }

  test("task 3.1 example") {
    val inputFile = "src/test/resources/task_3_example.txt"
    val result = AdventOfCode.task_3_1(inputFile)
    assertEquals(result, 157)
  }

  test("task 3.2 example") {
    val inputFile = "src/test/resources/task_3_example.txt"
    val result = AdventOfCode.task_3_2(inputFile)
    assertEquals(result, 70)
  }

  test("task 4.1 example") {
    val inputFile = "src/test/resources/task_4_example.txt"
    val result = AdventOfCode.task_4_1(inputFile)
    assertEquals(result, 2)
  }

  test("task 4.2 example") {
    val inputFile = "src/test/resources/task_4_example.txt"
    val result = AdventOfCode.task_4_2(inputFile)
    assertEquals(result, 4)
  }

  test("task 5.1 example") {
    val inputFile = "src/test/resources/task_5_example.txt"
    val result = AdventOfCode.task_5_1(inputFile)
    assertEquals(result, "CMZ")
  }

  test("task 5.2 example") {
    val inputFile = "src/test/resources/task_5_example.txt"
    val result = AdventOfCode.task_5_2(inputFile)
    assertEquals(result, "MCD")
  }

  test("task 6.1 example 1") {
    val inputFile = "src/test/resources/task_6_example_1.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 7)
  }

  test("task 6.1 example 2") {
    val inputFile = "src/test/resources/task_6_example_2.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 5)
  }

  test("task 6.1 example 3") {
    val inputFile = "src/test/resources/task_6_example_3.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 6)
  }

  test("task 6.1 example 4") {
    val inputFile = "src/test/resources/task_6_example_4.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 10)
  }

  test("task 6.1 example 5") {
    val inputFile = "src/test/resources/task_6_example_5.txt"
    val result = AdventOfCode.task_6_1(inputFile)
    assertEquals(result, 11)
  }

  test("task 6.2 example 1") {
    val inputFile = "src/test/resources/task_6_example_1.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 19)
  }

  test("task 6.2 example 2") {
    val inputFile = "src/test/resources/task_6_example_2.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 23)
  }

  test("task 6.2 example 3") {
    val inputFile = "src/test/resources/task_6_example_3.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 23)
  }

  test("task 6.2 example 4") {
    val inputFile = "src/test/resources/task_6_example_4.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 29)
  }

  test("task 6.2 example 5") {
    val inputFile = "src/test/resources/task_6_example_5.txt"
    val result = AdventOfCode.task_6_2(inputFile)
    assertEquals(result, 26)
  }

  test("task 7.1 example") {
    val inputFile = "src/test/resources/task_7_example.txt"
    val result = AdventOfCode.task_7_1(inputFile)
    assertEquals(result, 95437)
  }

  test("task 7.2 example") {
    val inputFile = "src/test/resources/task_7_example.txt"
    val result = AdventOfCode.task_7_2(inputFile)
    assertEquals(result, 24933642)
  }
}
