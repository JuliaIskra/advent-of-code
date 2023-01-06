import scala.io.Source
import scala.util.Using

object Task_12 {

  private def getPositionOf(heightmap: Array[Array[Char]], c: Char): (Int, Int) =
    val rows = heightmap.length
    val cols = heightmap.head.length
    var position = (-1, -1)
    (0 until rows)
      .foreach(row =>
        (0 until cols)
          .foreach(col =>
            if (heightmap(row)(col) == c)
              position = (row, col)
          )
      )
    position

  private def getHeightAt(heightmap: Array[Array[Char]], position: (Int, Int)): Char =
    val value = heightmap(position._1)(position._2)
    if (value == 'S') {
      'a'
    } else if (value == 'E') {
      'z'
    } else {
      value
    }

  private def findPossiblePaths(
      heightmap: Array[Array[Char]],
      startPosition: (Int, Int),
      visitedPositions: Set[(Int, Int)],
      whatNeighboursFit: (Char, Char) => Boolean
  ): Set[(Int, Int)] =
    val rows = heightmap.length
    val cols = heightmap.head.length
    val neighbourDiffCoordinates = Array((-1, 0), (1, 0), (0, 1), (0, -1))
    var possiblePaths = Set[(Int, Int)]()

    neighbourDiffCoordinates.foreach(diff =>
      val neighbour = (startPosition._1 + diff._1, startPosition._2 + diff._2)
      if (
        0 <= neighbour._1 && neighbour._1 < rows && 0 <= neighbour._2 && neighbour._2 < cols
        && !visitedPositions.contains(neighbour)
      ) {
        val currentHeight = getHeightAt(heightmap, startPosition)
        val neighbourHeight = getHeightAt(heightmap, neighbour)
        if (whatNeighboursFit(neighbourHeight, currentHeight)) {
          possiblePaths = possiblePaths + neighbour
        }
      }
    )
    possiblePaths

  private def findMinStepsTo(
      heightmap: Array[Array[Char]],
      startPosition: (Int, Int),
      whatNeighboursFit: (Char, Char) => Boolean,
      whenToStop: Set[(Int, Int)] => Boolean
  ): Int =
    var steps = 0
    var currentPositions = Set(startPosition)
    var visitedPositions = Set[(Int, Int)]()

    while (!whenToStop(currentPositions)) {
      var nextSteps = Set[(Int, Int)]()
      currentPositions.foreach(pos =>
        val paths = findPossiblePaths(
          heightmap,
          pos,
          visitedPositions,
          whatNeighboursFit
        )
        if (paths.nonEmpty) {
          nextSteps = nextSteps ++ paths
        }
      )
      visitedPositions = visitedPositions ++ currentPositions
      currentPositions = nextSteps
      steps += 1
    }
    steps

  def part_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val heightmap = source.getLines
        .map(_.toCharArray)
        .toArray

      val start = getPositionOf(heightmap, 'S')
      val destination = getPositionOf(heightmap, 'E')

      findMinStepsTo(
        heightmap,
        start,
        (neighbourHeight, currentHeight) => neighbourHeight - currentHeight <= 1,
        currentPositions => currentPositions.isEmpty || currentPositions.contains(destination)
      )
    }.get

  def part_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val heightmap = source.getLines
        .map(_.toCharArray)
        .toArray

      val start = getPositionOf(heightmap, 'E')

      findMinStepsTo(
        heightmap,
        start,
        (neighbourHeight, currentHeight) => currentHeight - neighbourHeight <= 1,
        currentPositions =>
          currentPositions.isEmpty
            || currentPositions.exists(pos => getHeightAt(heightmap, pos) == 'a')
      )
    }.get
}
