package Chainsaw.arithmetic

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

/** long multiplier implemented by divide-and-conquer
  *
  * @param bmSolution
  *   blueprint to build a big multiplier by divide-and-conquer method
  */
case class Bm(override val bmSolution: BmSolution)
    extends BmAlgo(bmSolution)
    with UnsignedMultiplier
    with OverwriteLatency {

  override def latency() = 1 // for implNaiveH only, as the latency is not determined before retiming

  override def name =
    s"${if (isConstantMult) "constant" else "variable"}_${className(multiplierType)}_${bmSolution.widthFull}"

  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawOperatorModule(this) {

    /** -------- operations in Bm
      * --------
      */
    def splitN(x: WeightedUInt, n: Int): collection.IndexedSeq[WeightedUInt] = {
      val values = x.value.subdivideIn(n slices) // todo: low to high?
      values
        .zip(x.arithInfo.splitN(n))
        .map { case (value, arithInfo) => WeightedUInt(value, arithInfo) }
    }

    def splitMSB(x: WeightedUInt) = {
      val (msb, main)                   = x.value.splitAt(x.value.getWidth - 1)
      val (msbArithInfo, mainArithInfo) = x.arithInfo.splitMsb
      (WeightedUInt(msb.asUInt, msbArithInfo), WeightedUInt(main.asUInt, mainArithInfo))
    }

    def add(v0: WeightedUInt, v1: WeightedUInt, constant: Boolean = false) = {
      require(v0.arithInfo.width == v1.arithInfo.width, s"v0: ${v0.arithInfo}, v1: ${v1.arithInfo}")
      val cpa = Cpa(BinaryAdder, v0.arithInfo.width)
      WeightedUInt(value = cpa.sum(v0.value, v1.value), arithInfo = v0.arithInfo + v1.arithInfo)
    }

    // TODO: using constant mult according to the threshold
    def mult(v0: WeightedUInt, v1: WeightedUInt) = {
      val sm = BaseDspMult(v0.arithInfo.width, v1.arithInfo.width)
      WeightedUInt(value = sm.prod(v0.value, v1.value), arithInfo = v0.arithInfo * v1.arithInfo)
    }

    def and(v0: WeightedUInt, v1: WeightedUInt) = {
      val ret = Mux(v1.value.asBool, v0.value, v0.value.getZero).d()
      WeightedUInt(value = ret, arithInfo = v0.arithInfo & v1.arithInfo)
    }

    // TODO: using unaligned version in rewriting phase
    def merge(weightedUInts: Seq[WeightedUInt], widthOut: Int): WeightedUInt = {
      val base  = weightedUInts.map(_.arithInfo.weight).min
      val merge = Merge(weightedUInts.map(_.arithInfo.withTime(0)))
      val ret   = merge.sum(weightedUInts.map(_.value))
      WeightedUInt(value = ret.resize(widthOut), arithInfo = ArithInfo(widthOut, base))
    }

    // TODO: clarify naming space
    def doRectangular(x: WeightedUInt, y: WeightedUInt, bmSolution: BmSolution): WeightedUInt = {
      if (bmSolution.isEmpty) {
        if (x.arithInfo.weight + y.arithInfo.weight < weightMax) mult(x, y)
        else WeightedUInt(0, ArithInfo(0, x.arithInfo.weight + y.arithInfo.weight))
      } else {
        val current = bmSolution.topDecomposition
        import current.{multiplierType => _, widthOut => _, _}
        val aWords = splitN(x, aSplit) // width = baseHeight
        val bWords = splitN(y, bSplit) // width = baseWidth

        def doNSplit(aWords: Seq[WeightedUInt], bWords: Seq[WeightedUInt]): Seq[WeightedUInt] = {
          bmSolution.multiplierType match {
            case FullMultiplier =>
              if (isKara) {
                val diagonals: Seq[WeightedUInt] = (0 until split).map { i =>
                  doRectangular(aWords(i), bWords(i), bmSolution.subSolution(bmSolution.multiplierType))
                }

                val prods: Seq[WeightedUInt] = Seq
                  .tabulate(split, split) { (i, j) =>
                    if (i > j) { // upper triangular, generated by karatsuba method
                      // pre-addition
                      val weight = aWords(i).arithInfo.weight + bWords(j).arithInfo.weight
                      require(
                        aWords(i).arithInfo.weight + bWords(j).arithInfo.weight == aWords(j).arithInfo.weight + bWords(
                          i
                        ).arithInfo.weight
                      )
                      val combinedA     = add(aWords(i), aWords(j))
                      val combinedB     = add(bWords(i), bWords(j), isConstantMult)
                      val (aMsb, aMain) = splitMSB(combinedA)
                      val (bMsb, bMain) = splitMSB(combinedB)
                      // sub-multiplication
                      val full = doRectangular(aMain, bMain, bmSolution.subSolution(FullMultiplier)).withWeight(weight)
                      val high = -diagonals(i).withWeight(weight)
                      val low  = -diagonals(j).withWeight(weight)
                      // full - high - low
                      val mainSegments = Seq(full, high, low)
                      // side-multiplications
                      val sideA        = and(bMain, aMsb).withWeight(weight + baseHeight)
                      val sideB        = and(aMain, bMsb).withWeight(weight + baseWidth)
                      val ab           = and(aMsb, bMsb).withWeight(weight + baseWidth + baseHeight)
                      val sideSegments = Seq(sideA, sideB, ab)
                      Some(mainSegments ++ sideSegments)
                    } else None
                  }
                  .flatten
                  .flatten
                  .flatten
                diagonals ++ prods
              } else {
                Seq
                  .tabulate(split, split) { (i, j) =>
                    doRectangular(aWords(i), bWords(j), bmSolution.subSolution(bmSolution.multiplierType))
                  }
                  .flatten
              }
            case SquareMultiplier =>
              Seq
                .tabulate(split, split) { (i, j) =>
                  if (i >= j) { // upper triangular
                    val multType = if (i == j) bmSolution.multiplierType else FullMultiplier
                    val prod     = doRectangular(aWords(i), bWords(j), bmSolution.subSolution(multType))
                    val ret      = if (i != j) prod << 1 else prod
                    Some(ret)
                  } else None
                }
                .flatten
                .flatten
            case MsbMultiplier =>
              Seq
                .tabulate(split, split) { (i, j) =>
                  if (i + j >= split - 1) {
                    val multType = if (i + j == split - 1) bmSolution.multiplierType else FullMultiplier
                    val ret      = doRectangular(aWords(i), bWords(j), bmSolution.subSolution(multType))
                    Some(ret)
                  } else None
                }
                .flatten
                .flatten
            case LsbMultiplier =>
              Seq
                .tabulate(split, split) { (i, j) =>
                  if (i + j <= split - 1) {
                    val multType = if (i + j == split - 1) bmSolution.multiplierType else FullMultiplier
                    val ret      = doRectangular(aWords(i), bWords(j), bmSolution.subSolution(multType))
                    Some(ret)
                  } else None
                }
                .flatten
                .flatten
          }
        }

        val segments = Seq
          .tabulate(factorB, factorA) { (i, j) => // for rectangular
            // distribute words to N-split sub modules
            val as = aWords.zipWithIndex.filter(_._2 % factorB == i).map(_._1).toSeq
            val bs = bWords.zipWithIndex.filter(_._2 % factorA == j).map(_._1).toSeq
            doNSplit(as, bs)
          }
          .flatten
          .flatten

        val validSegments = segments.filter(_.arithInfo.weight < weightMax)
        assert(validSegments.forall(_.arithInfo.width != 0))
        val ret = merge(validSegments, current.widthOut)
        ret
      }
    }

    // TODO: take care of constant width expansion
    val x = dataIn.head.asUInt()
    val y =
      if (isConstantMult) {
        multiplierType match {
          case MsbMultiplier => U(constant.get, widthY bits) << (widthX - widthY) // pad to right
          case _             => U(constant.get, widthX bits) // pad to left
        }
      } else dataIn.last.asUInt()
    val fullWeighted = doRectangular(
      WeightedUInt(x, ArithInfo(widthX, 0)),
      WeightedUInt(y, ArithInfo(bmSolution.widthFull, 0)),
      bmSolution
    )
    val fullRet = fullWeighted.value
    dataOut.head := (multiplierType match {
      case MsbMultiplier => fullRet.dropLow(widthOut - fullWeighted.arithInfo.weight).asUInt.resize(widthOut).toAFix
      case LsbMultiplier => fullRet.takeLow(widthOut).asUInt.toAFix
      case _             => fullRet.toAFix
    })
  }
}
