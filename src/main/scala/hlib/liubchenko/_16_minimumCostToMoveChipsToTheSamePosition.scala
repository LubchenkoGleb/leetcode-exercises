package hlib.liubchenko

object _16_minimumCostToMoveChipsToTheSamePosition extends App {
  def minCostToMoveChips(position: Array[Int]): Int = {
    val (evenCount, oddCount) = position.foldLeft((0, 0)) {
      case ((evenAcc, oddAcc), next) =>
        if (next % 2 == 0) (evenAcc + 1, oddAcc) else (evenAcc, oddAcc + 1)
    }

    math.min(evenCount, oddCount)
  }

  println(minCostToMoveChips(Array(2, 2, 2, 3, 3)))
}
