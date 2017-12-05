package org.umass.ciir

object miscLib {
  def time[R](block: => R, msg: String = ""): R = {
    print(msg + "\t")
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }


  implicit class ImplDoubleVecUtils(values: Iterable[Double]) {
    def mean = values.sum / values.size
  }
  implicit class ImplIntVecUtils(values: Iterable[Int]) {
    def mean = values.sum.toDouble / values.size
  }
}
