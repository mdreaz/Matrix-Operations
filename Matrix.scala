object Matrix {

  type Matrix = List[List[Int]]

  // Given a row and component number, n in the range 1..n,
  // returns the nth component of the row
  // e.g. extractComponent(List(4,3,5,7,8),3) = 5
  def extractComponent(row: List[Int], n: Int): Int = row match {
    case Nil => -1
    case x :: xs => if (n == 1) x else extractComponent(xs,n-1)
  }

  // Given a matrix m, and column number n in the range 1..n,
  // returns the nth column from the matrix
  // e.g. extractColumn(List(List(1,2,3),List(4,5,6)),3) = List(3,6)
  def extractColumn(m: Matrix, n: Int): List[Int] = 
    for {i <- m} yield extractComponent(i,n)

  // Similar to extractComponent except this function returns the row
  // after removing the nth column
  // removeComponent(List(4,3,5,7,8),3) = List(4,3,7,8)
  def removeComponent(row: List[Int], n: Int): List[Int] = row match {
    case Nil => Nil
    case x :: xs => if (n==1) xs else x :: removeComponent(xs,n-1)
  }


  // Returns the given matrix
  // after removing the nth column
  // e.g. removeColumn(List(List(1,2,3),List(4,5,6)),1) = List(List(2,3),List(5,6))
  def removeColumn(m: Matrix, n: Int): Matrix = 
    for {i <- m} yield removeComponent(i,n)


  // rows become columns and columns become rows
  // e.g. m1 = List(List(1,2,3),List(4,5,6))
  // transpose(m1) = List(List(1, 4), List(2, 5), List(3, 6))
  def transpose(m: Matrix): Matrix = 
    for (x <- (1 to m(1).length).toList ) yield extractColumn(m,x)

  // Computes the scalar product.
  // xs = List(3,4,7), ys = List(2,9,10)
  // scalarProduct(xs,ys) = 3*2+4*9+7*10 = 112
  def scalarProduct(xs: List[Int], ys: List[Int]): Int =
    (for ((x,y) <- (xs zip ys)) yield x * y).sum

  // Produces one row of the matrix multiplication
  def rowMul(row: List[Int], n: Matrix): List[Int] =
    for (x <- n) yield scalarProduct(row,x)

  // helper function which does the actual matrix multiplication
  // n would be the transpose of the second matrix in the multiplication
  def mmul(m: Matrix, n: Matrix): Matrix =
    for (i <- m) yield rowMul(i,n)

  // Multiplies two matrices
  // m2 = List(List(1,2),List(3,4))
  // m3 = List(List(5,6),List(7,8))
  // matrixMultiply(m2,m3) = List(List(19, 22), List(43, 50))
  // m4 = List(List(2,3,4),List(5,6,7))
  // m5 = List(List(1,2,3,4),List(2,3,5,6),List(7,1,2,4))
  // matrixMultiply(m4,m5) = List(List(36, 17, 29, 42), List(66, 35, 59, 84))
  // this function calls mmul with m and transpose(n)
  def matrixMultiply(m: Matrix, n: Matrix): Matrix = 
    mmul(m,transpose(n))

  
  // Computes the determinant for any square matrix
  // determinant(List(List(6,1,1),List(4,-2,5),List(2,8,7)) = -306
  def determinant(m: Matrix): Int = m match{
    case Nil           => 0
    case x :: Nil      => x(0)
    case x :: y :: Nil => x(0)*y(1)-x(1)*y(0)
    case x :: xs       => altSum(multi(m),1)
  }

  def multi(m: Matrix): List[Int] = {
    for (i <- (1 to m(0).length).toList) yield m(0)(i-1)*determinant(removeRow(removeColumn(m,i),1))
  }

  def altSum(lst: List[Int], sign: Int): Int = lst match {
    case Nil => 0
    case x :: xs => if (sign > 1) x+altSum(xs,-1) else x-altSum(xs,+1)
  }

  def removeRow(m: Matrix, n: Int): Matrix = m match {
    case Nil => Nil
    case x :: xs => if (n == 1) xs else x :: removeRow(xs,n-1)
  }

}
