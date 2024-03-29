package y2022

import scala.io.Source
import scala.util.Using

object Task_13 {

  private def parsePackets(inputFile: String): List[List[Either[Char, Int]]] =
    Using(Source.fromFile(inputFile)) { source =>
      source
        .getLines()
        .filterNot(_.isEmpty)
        .map(line =>
          line
            .foldLeft((List[Either[Char, Int]](), ""))((state, c) =>
              val (list, nAcc) = state
              c match {
                case ',' =>
                  if (nAcc.isEmpty) (list, nAcc)
                  else (list :+ Right(nAcc.toInt), "")
                case '[' => (list :+ Left('['), nAcc)
                case ']' =>
                  if (nAcc.isEmpty) (list :+ Left(']'), nAcc)
                  else (list :+ Right(nAcc.toInt) :+ Left(']'), "")
                case n => (list, nAcc + n)
              }
            )
            ._1
        )
        .toList
    }.get

  private def isLessThen: (List[Either[Char, Int]], List[Either[Char, Int]]) => Boolean =
    (packet_1, packet_2) =>
      var p_1 = packet_1
      var p_2 = packet_2
      var p1_idx = 0
      var p2_idx = 0
      var inOrder = Option.empty[Boolean]

      while (p1_idx < p_1.size && p2_idx < p_2.size && inOrder.isEmpty) {
        (p_1(p1_idx), p_2(p2_idx)) match {
          case (Left('['), Left('[')) | (Left(']'), Left(']')) =>
            p1_idx += 1
            p2_idx += 1
          case (Left('['), Right(n)) =>
            p_2 = p_2.take(p2_idx) :+ Left('[') :+ Right(n) :+ Left(']') :++ p_2.drop(
              p2_idx + 1
            )
          case (Left(']'), _) =>
            inOrder = Some(true)
          case (Right(n), Left('[')) =>
            p_1 = p_1.take(p1_idx) :+ Left('[') :+ Right(n) :+ Left(']') :++ p_1.drop(
              p1_idx + 1
            )
          case (_, Left(']')) =>
            inOrder = Some(false)
          case (Right(n1), Right(n2)) if n1 == n2 =>
            p1_idx += 1
            p2_idx += 1
          case (Right(n1), Right(n2)) if n1 > n2 =>
            inOrder = Some(false)
          case (Right(n1), Right(n2)) if n1 < n2 =>
            inOrder = Some(true)
          case (l, r) => throw new RuntimeException(s"Unaccountable pair: $l, $r")
        }
      }
      inOrder.get

  def part_1(inputFile: String): Int =
    val packets = parsePackets(inputFile)

    var idx = 0
    var pairsInOrder = List[Int]()
    while (idx < packets.size) {
      if (isLessThen(packets(idx), packets(idx + 1))) {
        pairsInOrder = pairsInOrder :+ (idx + 2) / 2
      }
      idx += 2
    }
    pairsInOrder.sum

  def part_2(inputFile: String): Int =
    val dividerPacket_1 =
      List[Either[Char, Int]](Left('['), Left('['), Right(2), Left(']'), Left(']'))
    val dividerPacket_2 =
      List[Either[Char, Int]](Left('['), Left('['), Right(6), Left(']'), Left(']'))

    val packets = (parsePackets(inputFile) :+ dividerPacket_1 :+ dividerPacket_2)
      .sortWith(isLessThen)

    val idx_1 = packets.indexOf(dividerPacket_1) + 1
    val idx_2 = packets.indexOf(dividerPacket_2) + 1
    idx_1 * idx_2
}
