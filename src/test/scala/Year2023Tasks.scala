import y2023.*

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
}