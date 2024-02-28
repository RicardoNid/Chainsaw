package Chainsaw.algorithms

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

/** a basic transform with concrete implementation "transform"
  */
abstract class Transform[T] {

  val sizeIn: Int
  val sizeOut: Int
  def transform(dataIn: Seq[T]): Seq[T]
  def symbol: String

  // TODO: an extra "symbol" method, so that it can be displayed properly in a TransformList

}

/** a transform step, which is a sequence of transforms executed in parallel
  */
class TransformStep[T](val transforms: Seq[Transform[T]]) {

  def splitBySizes(seq: Seq[T], sizes: Seq[Int]): Seq[Seq[T]] = {
    var remain: Seq[T] = seq
    sizes.map { size =>
      val (out, rem) = remain.splitAt(size)
      remain = rem
      out
    }
  }
  def transform(dataIn: Seq[T]): Seq[T] = {
    val sizes = transforms.map(_.sizeIn)
    assert(sizes.sum == dataIn.size)
    splitBySizes(dataIn, sizes)
      .zip(transforms)
      .map { case (dataIn, transform) => transform.transform(dataIn) }
      .reduce(_ ++ _) // concatenate
  }

  def sizeIn: Int  = transforms.map(_.sizeIn).sum
  def sizeOut: Int = transforms.map(_.sizeOut).sum

  override def toString: String = transforms.map(_.symbol).mkString("++")

  /** indicate how this step should be folded to fit the target parallelism
    * @param parallelism
    * @return
    */
  def getFolded(parallelism: Int): String = {
    val errorInfo =
      s"fold failed, transform sizes are ${transforms.map(_.sizeIn).mkString(", ")}, parallelism is $parallelism"
    // get segments
    val segments = ArrayBuffer[ArrayBuffer[Transform[T]]]()
    var remained = parallelism
    transforms.foreach { transform => // TODO: better method to get segments
      if (transform.sizeIn >= parallelism) {
        require(remained == parallelism && transform.sizeIn % parallelism == 0, errorInfo)
        segments.append(ArrayBuffer[Transform[T]](transform))
      } else {
        require(remained >= transform.sizeIn, errorInfo)
        if (remained == parallelism) segments.append(ArrayBuffer[Transform[T]]())
        segments.last.append(transform)
        remained -= transform.sizeIn
        if (remained == 0) remained = parallelism
      }
    }

    val descriptions = segments.map { segment =>
      val t0 = segment.head
      if (t0.sizeIn <= parallelism) {
        if (segment.length > 1 && segment.forall(_ == t0)) s"${t0.toString} ⊗ ${segment.length}"
        else segment.map(_.symbol).mkString(" ++ ")
      } else s"${t0.symbol} folded by ${t0.sizeIn / parallelism}"
    }

    if (descriptions.forall(_ == descriptions.head)) descriptions.head
    else descriptions.mkString(" | ")

  }

}

/** a transform list, which is a sequence of transform steps executed in sequence
  */
class TransformList[T](val steps: Seq[TransformStep[T]]) {
  def transform(dataIn: Seq[T]): Seq[T] = steps.foldLeft(dataIn)((data, transform) => transform.transform(data))

  def sizeIn: Int  = steps.head.sizeIn
  def sizeOut: Int = steps.last.sizeOut

  def *(that: TransformList[T]) = {
    assert(this.sizeOut == that.sizeIn, s"size mismatch, ${this.sizeOut} != ${that.sizeIn}")
    new TransformList[T](this.steps ++ that.steps)
  }

  // transform 'arithmetics', using these methods, we can construct a complicated transform by simple ones
  def ^(times: Int) = Seq.fill(times)(this).reduce(_ * _)

  def ++(that: TransformList[T]) = {
    assert(this.steps.size == that.steps.size)
    val newSteps = this.steps.zip(that.steps).map { case (thisStep, thatStep) =>
      new TransformStep[T](thisStep.transforms ++ thatStep.transforms)
    }
    new TransformList[T](newSteps)
  }

  def kronecker(times: Int): TransformList[T] = Seq.fill(times)(this).reduce(_ ++ _)
  def ⊗(times: Int): TransformList[T]         = kronecker(times) // kronecker product

  // visualization methods
  // TODO: better visualization
  def printSteps() = println(toString)

  def foreachStep(f: TransformStep[T] => Unit) = steps.foreach(f)

  def foreachTransform(f: Transform[T] => Unit) = steps.foreach(_.transforms.foreach(f))

  override def toString: String = {
    val widthFactorDouble = steps.flatMap(_.transforms.map(t => t.symbol.length / t.sizeIn.toDouble)).max
    val widthFactor       = scala.math.ceil(widthFactorDouble).toInt
    def getPadded(transform: Transform[T]) = {
      val space = transform.sizeIn * widthFactor
      val left  = (space - transform.symbol.length) / 2
      val right = space - left - transform.symbol.length
      " " * left + transform.symbol + " " * right
    }
    steps.map(_.transforms.map(getPadded).mkString("")).mkString("\n")
  }

  def drawSteps() = {
    // TODO: draw the schematic by drawio
  }

  /** indicate how this list should be folded to fit the target parallelism
    */
  def printFolded(parallelism: Int) = println(
    s"""
       |////////////////
       |//folded for $parallelism
       |////////////////
       |${steps.map(_.getFolded(parallelism)).mkString("\n")}
       |""".stripMargin
  )

//  override def toString: String = s"steps:\n${steps.mkString("\n")}"
}
