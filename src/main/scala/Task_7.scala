import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Task_7 {

  private def parseDirs(inputFile: String): mutable.Map[String, Int] =
    Using(Source.fromFile(inputFile)) { source =>
      val dirs = mutable.Map[String, Int]()
      var path = Array[String]()
      source.getLines
        .foreach(line => {
          if (line.startsWith("$")) {
            line match {
              case "$ ls" => // do nothing
              case cdDir =>
                cdDir.substring(5) match {
                  case "/" =>
                    dirs.put("root", 0)
                    path = Array("root")
                  case ".." =>
                    path = path.take(path.length - 1)
                  case dir =>
                    val fullDirName = path.reduce(_ + "/" + _) + "/" + dir
                    dirs.put(fullDirName, 0)
                    path = path.appended(dir)
                }
            }
          } else {
            val split = line.split(" ")
            (split(0), split(1)) match {
              case ("dir", dir) =>
                val fullDirName = path.reduce(_ + "/" + _) + "/" + dir
                dirs.put(fullDirName, 0)
              case (size, _) =>
                var fullDirName = ""
                path
                  .foreach(dir => {
                    fullDirName = if (fullDirName == "") dir else fullDirName + "/" + dir
                    val currentSize = dirs(fullDirName)
                    dirs.put(fullDirName, currentSize + size.toInt)
                  })
            }
          }
        })
      dirs
    }.get

  def part_1(inputFile: String): Int =
    val dirs = parseDirs(inputFile)
    dirs.filter((_, size) => size <= 100000).values.sum

  def part_2(inputFile: String): Int =
    val totalSpace = 70000000
    val neededSpace = 30000000

    val dirs = parseDirs(inputFile)

    val usedSpace = dirs("root")
    val spaceToFree = neededSpace - (totalSpace - usedSpace)

    dirs.values.foldLeft(usedSpace)((bestToRemove, dirSize) =>
      if (dirSize > spaceToFree && dirSize < bestToRemove) {
        dirSize
      } else {
        bestToRemove
      }
    )
}
