package hlib.liubchenko.topinterview150.matrix

class _2_spiral_matrix {
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    def loop(top: Int, right: Int, bottom: Int, left: Int, acc: List[Int]): List[Int] = {
//      if(top >bottom || left > right) acc
//      else if(top == bottom) acc ++ (left to right).map(matrix(top))
//      else if(left == right) acc ++ ()
    }

//    var (topBorder, rightBorder, bottomBorder, leftBorder, direction, point) =
//      (1, matrix.length - 1, matrix.length - 1, 0, 'r', (0, 0))
//
//    (0 until matrix.length * matrix.length).toList.map{ case 0 => ()}
//
//    matrix.transpose()
//
    ???
  }

  //   1     2     3     4      5    6     7     8     9
  // [1,1] [1,2] [1,3] [2,3] [3,3] [3,2] [3,1] [2,1] [2,2]

  // [1] [2] [3]
  // [8] [9] [4]
  // [7] [6] [5]

  // [1]  [2]  [3]  [4]
  // [12] [13] [14] [5]
  // [11] [16] [15] [6]
  // [10] [9]  [8]  [7]


}
