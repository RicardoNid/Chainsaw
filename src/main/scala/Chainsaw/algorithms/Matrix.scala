package Chainsaw.algorithms

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

case class RowOperation[T: ClassTag](operation: String, rowId0: Int, rowId1: Int, multiple: T)(implicit
    field: Field[T]
) {
  import field._
  def inverse: RowOperation[T] = operation match {
    case "replace"     => RowOperation("replace", rowId0, rowId1, -multiple)
    case "interchange" => RowOperation("interchange", rowId1, rowId0, multiple)
    case "scale"       => RowOperation("scale", rowId0, rowId1, field.one / multiple)
  }

  def bias(base: Int) = RowOperation(operation, rowId0 + base, rowId1 + base, multiple)
}

/** Represents a matrix with elements of type T.
  *
  * @param data
  *   The data of the matrix stored in a two-dimensional array, where the first dimension represents the rows and the
  *   second dimension represents the columns.
  * @param field
  *   The type class providing the necessary operations for performing calculations with the elements of the matrix.
  * @tparam T
  *   The type of the elements in the matrix.
  */
case class Matrix[T: ClassTag](data: Array[Array[T]])(implicit field: Field[T]) {

  import field._

  ////////////////////
  // basic attributes
  ////////////////////

  def rowCount: Int = data.length
  def colCount: Int = data.head.length

  private def _getRows: Array[Array[T]]    = data
  private def _getColumns: Array[Array[T]] = data.transpose

  private def _getRow(index: Int): Array[T]    = data(index)
  private def _getColumn(index: Int): Array[T] = data.map(_(index))

  def getRow(index: Int): Matrix[T]    = Matrix.row(_getRow(index): _*)
  def getColumn(index: Int): Matrix[T] = Matrix.column(_getColumn(index): _*)

  def subMatrix(rowRange: Range, columnRange: Range) = {
    val rows    = data.slice(rowRange.start, rowRange.end)
    val subData = rows.map(row => row.slice(columnRange.start, columnRange.end))
    Matrix(subData)
  }

  ////////////////////
  // elementary row operations
  ////////////////////

  /** Performs an elementary row operation on the matrix.
    *
    * @param rowOperation
    *   the row operation to perform
    *   - operation: the type of operation to perform (replace, interchange, scale)
    *   - rowId0: the index of the first row involved in the operation
    *   - rowId1: the index of the second row involved in the operation
    *   - multiple: the multiple to apply to the second row (only applicable for replace and scale operations)
    * @return
    *   the resulting matrix after the row operation is applied, without modifying the original matrix
    */
  def withElementaryRowOperation(rowOperation: RowOperation[T]): Matrix[T] = {
    import rowOperation._
    val newRows = (0 until rowCount).map { i =>
      val row = _getRow(i)
      rowOperation.operation match {
        case "replace" => // row replacement: row0 -> row0 + row1 * multiple
          if (i == rowId0) row.zip(_getRow(rowId1)).map { case (a, b) => a + b * multiple }
          else row
        case "interchange" => // row interchange: row0 <-> row1
          if (i == rowId0) _getRow(rowId1)
          else if (i == rowId1) _getRow(rowId0)
          else row
        case "scale" => // row scaling: row0 -> row0 * multiple
          if (i == rowId0) row.map(_ * multiple)
          else row
      }
    }
    Matrix(newRows.toArray)
  }

  ////////////////////
  // Gaussian elimination
  ////////////////////

  def getLeadingEntryIndex(rowId: Int): Int  = _getRow(rowId).indexWhere(_ != field.zero)
  def getLeadingEntry(rowId: Int): Option[T] = _getRow(rowId).find(_ != field.zero)

  /** Checks whether the table is in row echelon form (REF).
    */
  def isRef: Boolean = {
    val entryLine = (0 until rowCount)
      .map(getLeadingEntryIndex)
      .map(i => if (i == -1) colCount else i) // row with no leading entry should be sorted to the end
    val shortenedLine = entryLine.reverse.dropWhile(_ == colCount).reverse
    shortenedLine.zip(shortenedLine.tail).forall { case (a, b) => a < b }
  }

  /** Checks if the matrix is in reduced echelon form(RREF).
    */
  def isRref: Boolean = {
    val isOne = (0 until rowCount).flatMap(getLeadingEntry).forall(_ == field.one) // all leading entries are 1
    val isOnly = (0 until rowCount)
      .map(getLeadingEntryIndex)
      .filter(_ != -1)
      .forall(i =>
        _getColumn(i).count(_ != field.zero) == 1
      ) // all leading entries are the only non-zero entry in their columns
    isRef && isOne && isOnly
  }

  /** Get row echelon form of the matrix and returns it along with the elementary row operations performed.
    *
    * @param rowBase
    *   Recursion depth.
    * @return
    *   A tuple containing the row echelon form of the matrix and the row operations applied.
    */
  private def _getRef(rowBase: Int = 0): (Matrix[T], ArrayBuffer[RowOperation[T]]) = {
    val operations = ArrayBuffer[RowOperation[T]]() // recording the operations
    if (data.forall(_.forall(_ == field.zero))) (this, operations)
    else {
      var ret = this
      // step 1: find leftmost non-zero column
      val colId = _getColumns.indexWhere(_.exists(_ != field.zero))
      // step 2: select pivot and do row interchange to move the pivot to the top
      val pivotRowId = _getColumn(colId).indexWhere(
        _ != field.zero
      ) // select the first non-zero entry as pivot TODO: partial pivoting
      val op = RowOperation("interchange", 0, pivotRowId, field.zero)
      ret = ret.withElementaryRowOperation(op)
      operations += op.bias(rowBase)
      // step 3: do row replacements to create zeros in all positions below the pivot
      (1 until rowCount).foreach { i =>
        val multiple = -ret._getRow(i)(colId) / ret._getRow(0)(colId)
        val op       = RowOperation("replace", i, 0, multiple)
        ret = ret.withElementaryRowOperation(op)
        operations += op.bias(rowBase)
      }

      val subMatrix               = Matrix(ret._getRows.tail)
      val (subRet, subOperations) = subMatrix._getRef(rowBase + 1) // recursive call on the sub-matrix
      val rows                    = ret._getRow(0) +: subRet._getRows
      operations ++= subOperations
      (Matrix(rows), operations)
    }
  }

  /** Get reduced row echelon form of the matrix and returns it along with the elementary row operations performed.
    *
    * @return
    *   A tuple containing the matrix in reduced row echelon form and the row operations applied.
    */
  private def _getRref: (Matrix[T], ArrayBuffer[RowOperation[T]]) = {
    var ret               = this
    val (ref, operations) = ret._getRef()
    ret = ref
    ret.getPivotPositions.foreach { case (i, j) =>
      // step 1: create a 1 in the pivot position
      val pivot    = ret._getRow(i)(j)
      val multiple = field.one / pivot
      val op       = RowOperation("scale", i, 0, multiple)
      ret = ret.withElementaryRowOperation(op)
      operations += op
      // step 2: create zeros above the pivot
      (0 until i).foreach { k =>
        val multiple = -ret._getRow(k)(j)
        val op       = RowOperation("replace", k, i, multiple)
        ret = ret.withElementaryRowOperation(op)
        operations += op
      }
    }

    (ret, operations)
  }

  def getRef: Matrix[T]  = _getRef()._1
  def getRref: Matrix[T] = _getRref._1

  /** Get the pivot positions in the matrix.
    *
    * @return
    *   Array of tuples containing the row and column indices of the pivot positions.
    */
  def getPivotPositions: Array[(Int, Int)] = {
    val matrix = if (isRef) this else getRef
    (0 until rowCount).map(i => (i, matrix.getLeadingEntryIndex(i))).filter(_._2 != -1).toArray
  }

  def getPivotColumns: Array[Matrix[T]] = getPivotPositions.map(_._2).map(getColumn)

  /** Judge whether the linear system is consistent, when this is the augmented matrix of the linear system.
    */
  def isConsistent = !getPivotPositions.map(_._2).contains(colCount - 1)

  /** Judge whether the linear system has only one solution, when this is the augmented matrix of the linear system.
    */
  def isUnique = isConsistent && getPivotColumns.length == colCount - 1

  /** Try to solve the linear equation this * x = target, where both x and target are column vectors
    */
  def solve(target: Matrix[T]): Option[Matrix[T]] = { // TODO: verify the correctness
    require(target.colCount == 1 && target.rowCount == rowCount)
    val N         = colCount // number of variables
    val augmented = Matrix(data.zip(target.data).map { case (row1, row2) => row1 ++ row2 })
    val ref       = augmented.getRef
    if (ref.isConsistent) {
      if (ref.isUnique) {
        Some(
          ref.getRref.subMatrix(rowRange = 0 until N, columnRange = N until (N + 1))
        ) // the first N-1 entries of the last column are the solutions
      } else {
        println("the linear system has infinite many solutions")
        Some(ref.getRref) // TODO: return one of the solutions and print free variables
      }
    } else {
      println("the linear system has no solution")
      None
    }
  }

  ////////////////////
  // Definitions of special matrices
  ////////////////////
  def isSquare: Boolean = rowCount == colCount

  def isUpperTriangular: Boolean =
    Seq
      .tabulate(rowCount, colCount) { (i, j) =>
        if (i > j) data(i)(j) == field.zero else true
      }
      .flatten
      .forall(_ == true)

  def isLowerTriangular: Boolean = {
    Seq
      .tabulate(rowCount, colCount) { (i, j) =>
        if (i < j) data(i)(j) == field.zero else true
      }
      .flatten
      .forall(_ == true)
  }

  def isDiagonal: Boolean = isUpperTriangular && isLowerTriangular

  def isIdentity = isDiagonal && (0 until rowCount).forall(i => data(i)(i) == field.one)

  def isZero = data.flatten.forall(_ == field.zero)

  def isSymmetric: Boolean = this == transpose

  // TODO: norm and condition number
  // TODO: define permuted lower triangular matrix

  ////////////////////
  // Vector space, more methods are implemented by VectorSpace object
  ////////////////////

  /** get the k-dimensional vector space, for finite fields only
    * @param enum
    *   all elements in the field
    */
  def getVectorSpace(enum: Array[T], k: Int): Array[Matrix[T]] = {
    val all   = enum
    val radix = all.length
    val size  = Seq.fill(k)(radix).product
    (0 until size).map { i => // for all possible inputs
      var current = i
      val data = (0 until k).map { j =>
        val index = current % radix
        current = current / radix
        all(index) // get the corresponding element
      }.toArray
      Matrix.column(data: _*)
    }.toArray
  }

  /** get the image(column space) of a matrix, for finite fields only
    */
  def getImage(enum: Array[T]): Array[Matrix[T]] = getVectorSpace(enum, colCount).map(this * _).distinct

  def getColumnSpace(enum: Array[T]): Array[Matrix[T]] = getImage(enum)

  /** get the kernel(null space) of a matrix, for finite fields only
    */
  def getKernel(enum: Array[T]): Array[Matrix[T]] = {
    val zeroVector = Matrix.column(Seq.fill(colCount)(field.zero): _*)
    // TODO:better implementation to avoid traversing all vectors
    getVectorSpace(enum, colCount).filter(this * _ == zeroVector)
  }

  def getNullSpace(enum: Array[T]): Array[Matrix[T]] = getKernel(enum)

  ////////////////////
  // Matrix operations
  ////////////////////
  def +(that: Matrix[T]): Matrix[T] = {
    require(data.length == that.data.length && data.head.length == that.data.head.length)
    val newData = data.zip(that.data).map { case (row1, row2) => row1.zip(row2).map { case (a, b) => a + b } }
    Matrix(newData)
  }

  def -(that: Matrix[T]): Matrix[T] = {
    require(data.length == that.data.length && data.head.length == that.data.head.length)
    val newData = data.zip(that.data).map { case (row1, row2) => row1.zip(row2).map { case (a, b) => a - b } }
    Matrix(newData)
  }

  def *(that: Matrix[T]): Matrix[T] = {
    require(data.head.length == that.data.length)
    val newData = data.map { row1 =>
      that.data.transpose.map { row2 =>
        row1.zip(row2).map { case (a, b) => a * b }.sum
      }
    }
    Matrix(newData)
  }

  def ^(n: Int): Matrix[T] = {
    require(isSquare && n > 0)
    (1 until n).foldLeft(this)((acc, _) => acc * this)
  }

  // FIXME: failed on row vector
  def transpose: Matrix[T] = Matrix(data.transpose)

  ////////////////////
  // Rank and relative properties
  ////////////////////
  // FIXME: determine the rank by SVD rather than gaussian elimination
  def getRank: Int = getPivotPositions.length

  ////////////////////
  // Determinant and relative properties
  ////////////////////

  /** get determinant of a matrix by recursive definition
    * @return
    *   the determinant value of the matrix
    */
  def det: T = {
    require(isSquare, "determinant is only defined for square matrices")
    val size = data.length
    size match {
      case 1 => data(0)(0)
      case 2 => data(0)(0) * data(1)(1) - data(0)(1) * data(1)(0) // ad - bc
      case _ =>
        (0 until size).map { i =>
          val subMatrix: Matrix[T] = Matrix(data.tail.map(row => (row.take(i) ++ row.drop(i + 1)).toArray))
          val multiplier =
            if (i % 2 == 0) field.one
            else field.negate(field.one) // minus one
          multiplier * data(0)(i) * subMatrix.det
        }.sum
    }
  }

  ////////////////////
  // Inverse
  ////////////////////
  def isInvertible: Boolean = det != field.zero

  def getInverse: Matrix[T] = {
    require(isInvertible)
    val (_, operations) = _getRref
    var ret             = Matrix.identity[T](rowCount)
    operations.foreach { operation =>
      ret = ret.withElementaryRowOperation(operation)
    }
    ret
  }

  ////////////////////
  // Factorizations
  ////////////////////

  def getLU: (Matrix[T], Matrix[T]) = {
    val (u, operations)   = _getRef()
    val reverseOperations = operations.reverse.map(_.inverse)
    var l                 = Matrix.identity[T](rowCount)
    reverseOperations.foreach { operation =>
      l = l.withElementaryRowOperation(operation)
    }
    (l, u)
  }

  // TODO: implement SVD, QR, etc.

  def isPermutationMatrix: Boolean = {
    def isGood(vector: Array[T]) = vector.count(_ != field.zero) == 1 && vector.count(_ == field.one) == 1
    _getRows.forall(isGood) && _getColumns.forall(isGood)
  }

  override def toString: String = {
    val maxLength = data.flatten.map(_.toString.length).max
    "matrix:\n" + data
      .map(row => "[" + row.map(_.toString.padTo(maxLength, ' ')).mkString(" ") + "]")
      .mkString("\n")
  }

  // without this, method "distinct" will not work correctly
  override def hashCode(): Int = {
    java.util.Arrays.deepHashCode(data.map(_.toArray))
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Matrix[T] => java.util.Arrays.deepEquals(this.data.map(_.toArray), that.data.map(_.toArray))
    case _               => false
  }

}

object Matrix {

  def apply[T: ClassTag](data: Array[T], rowCount: Int, colCount: Int)(implicit numeric: Field[T]): Matrix[T] = {
    require(data.length == rowCount * colCount)
    new Matrix(data.grouped(colCount).toArray)
  }

  def row[T: ClassTag](data: T*)(implicit numeric: Field[T]): Matrix[T]    = new Matrix(Array(data.toArray))
  def column[T: ClassTag](data: T*)(implicit numeric: Field[T]): Matrix[T] = new Matrix(data.toArray.map(Array(_)))

  def identity[T: ClassTag](n: Int)(implicit numeric: Field[T]): Matrix[T] = {
    val data = Array.tabulate(n, n)((i, j) => if (i == j) numeric.one else numeric.zero)
    Matrix(data)
  }

}

object VectorSpace {}
