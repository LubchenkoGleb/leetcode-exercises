package hlib.liubchenko

import scala.collection.mutable.ArrayBuffer

object _12_availableCapturesForRook extends App {
  def numRookCaptures(board: Array[Array[Char]]): Int = {
    val rookPosition = for {
      i <- board.indices
      j <- board.indices
      if board(i)(j) == 'R'
    } yield (i, j)
    val (rLine, rCol) = rookPosition.head

    val pawnsBuilder = ArrayBuffer.newBuilder[(Int, Int)]
    board(rLine).zipWithIndex.foreach {
      case ('p', pCol)          => pawnsBuilder.addOne(rLine, pCol)
      case (_, _) =>
    }
    board.zipWithIndex.map { case (line, lI) => (line(rCol), lI) }.foreach {
      case ('p', pLine)         => pawnsBuilder.addOne(pLine, rCol)
      case (_, _) =>
    }
    val pawns = pawnsBuilder.result()

    pawns.count {
      case (line, col) if line == rLine && col < rCol =>
        board(line).slice(col + 1, rCol).forall(_ == '.')
      case (line, col) if line == rLine && col > rCol =>
        board(line).slice(rCol + 1, col).forall(_ == '.')
      case (line, col) if col == rCol && line < rLine =>
        board.slice(line + 1, rLine).forall(_(col) == '.')
      case (line, col) if col == rCol && line > rLine =>
        board.slice(rLine + 1, line).forall(_(col) == '.')
    }
  }

  val input: Array[Array[Char]] =
    """[
      |[".",".",".",".",".",".",".","."],
      |[".",".",".","p",".",".",".","."],
      |[".",".",".","p",".",".",".","."],
      |["p","p",".","R",".","p","B","."],
      |[".",".",".",".",".",".",".","."],
      |[".",".",".","B",".",".",".","."],
      |[".",".",".","p",".",".",".","."],
      |[".",".",".",".",".",".",".","."]
      |]""".stripMargin
      .replaceAll("[\\[\\]\",]", "")
      .trim
      .split("\n")
      .map(_.toCharArray)

  println(input.map(_.mkString(" ")).mkString("\n"))

  println(numRookCaptures(input))
}
