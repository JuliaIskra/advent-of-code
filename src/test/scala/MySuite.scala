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
}
