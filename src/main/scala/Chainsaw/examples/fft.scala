package Chainsaw.examples

case class Ntt(NMax: Int) {

  val depth = log2(NMax)
  assert(NMax == 1 << depth, s"NMax must be a power of 2, but $NMax is not")

  def log2(n: Int): Int = math.ceil(math.log10(n) / math.log10(2)).toInt

  def getAllFactors(n: Int): Seq[Int] = (1 to n).filter(n % _ == 0)
  def listParams() = {
    println("legal combinations:")
    val legalDepths = getAllFactors(depth)
    legalDepths.foreach(d => println(s"depth = $d, width minimum = ${1 << (d-1)}"))
  }

}

object Ntt {
  def main(args: Array[String]): Unit = {
    val ntt = Ntt(1024)
    ntt.listParams()
  }
}
