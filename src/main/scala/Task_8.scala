import scala.io.Source
import scala.util.Using

object Task_8 {

  def part_1(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val treesMap = source.getLines
        .map(line => line.map(c => c.toString.toInt))
        .map(_.toList)
        .foldLeft(List[List[Int]]())((list, a) => list.appended(a))

      val maxRow = treesMap.size - 1
      val maxCol = treesMap.head.size - 1
      var invisibleCount = 0

      treesMap.indices
        .foreach(row =>
          treesMap.head.indices
            .foreach(col =>
              // skip edge trees
              if (row != 0 && row != maxRow && col != 0 && col != maxCol) {
                val treeSize = treesMap(row)(col)

                var visibleFromAbove = true
                var aboveRow = row - 1
                while (visibleFromAbove && aboveRow >= 0) {
                  if (treeSize <= treesMap(aboveRow)(col)) {
                    visibleFromAbove = false
                  }
                  aboveRow -= 1
                }

                var visibleFromBelow = true
                var belowRow = row + 1
                while (visibleFromBelow && belowRow <= maxRow) {
                  if (treeSize <= treesMap(belowRow)(col)) {
                    visibleFromBelow = false
                  }
                  belowRow += 1
                }

                var visibleFromLeft = true
                var leftCol = col - 1
                while (visibleFromLeft && leftCol >= 0) {
                  if (treeSize <= treesMap(row)(leftCol)) {
                    visibleFromLeft = false
                  }
                  leftCol -= 1
                }

                var visibleFromRight = true
                var rightCol = col + 1
                while (visibleFromRight && rightCol <= maxCol) {
                  if (treeSize <= treesMap(row)(rightCol)) {
                    visibleFromRight = false
                  }
                  rightCol += 1
                }

                if (
                  !visibleFromAbove && !visibleFromBelow && !visibleFromLeft && !visibleFromRight
                ) {
                  invisibleCount += 1
                }
              }
            )
        )

      treesMap.size * treesMap.head.size - invisibleCount
    }.get

  def part_2(inputFile: String): Int =
    Using(Source.fromFile(inputFile)) { source =>
      val treesMap = source.getLines
        .map(line => line.map(c => c.toString.toInt))
        .map(_.toList)
        .foldLeft(List[List[Int]]())((list, a) => list.appended(a))

      val maxRow = treesMap.size - 1
      val maxCol = treesMap.head.size - 1
      var maxScore = 0

      treesMap.indices
        .foreach(row =>
          treesMap.head.indices
            .foreach(col =>
              val treeSize = treesMap(row)(col)
              var stop = false

              var visibleFromAbove = 0
              var aboveRow = row - 1
              while (!stop && aboveRow >= 0) {
                visibleFromAbove += 1
                if (treeSize <= treesMap(aboveRow)(col)) {
                  stop = true
                }
                aboveRow -= 1
              }

              stop = false
              var visibleFromBelow = 0
              var belowRow = row + 1
              while (!stop && belowRow <= maxRow) {
                visibleFromBelow += 1
                if (treeSize <= treesMap(belowRow)(col)) {
                  stop = true
                }
                belowRow += 1
              }

              stop = false
              var visibleFromLeft = 0
              var leftCol = col - 1
              while (!stop && leftCol >= 0) {
                visibleFromLeft += 1
                if (treeSize <= treesMap(row)(leftCol)) {
                  stop = true
                }
                leftCol -= 1
              }

              stop = false
              var visibleFromRight = 0
              var rightCol = col + 1
              while (!stop && rightCol <= maxCol) {
                visibleFromRight += 1
                if (treeSize <= treesMap(row)(rightCol)) {
                  stop = true
                }
                rightCol += 1
              }

              val visibilityScore =
                visibleFromAbove * visibleFromBelow * visibleFromLeft * visibleFromRight
              if (visibilityScore > maxScore) {
                maxScore = visibilityScore
              }
            )
        )

      maxScore
    }.get
}
