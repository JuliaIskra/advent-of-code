// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

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
}
