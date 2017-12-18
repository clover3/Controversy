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

  def topKBy[A,B <% Ordered[B]]( items : List[A], k: Int)( f : A => B) : List[(A,B)] = {
    val scores = items map f
    val sorted = (items zip scores).sortBy(_._2)
    sorted.reverse.slice(0,k)
  }
  def average(l : Iterable[Double]) : Double = l.sum / l.size

  implicit class ImplDoubleVecUtils(values: Iterable[Double]) {
    def mean = values.sum / values.size
  }
  implicit class ImplIntVecUtils(values: Iterable[Int]) {
    def mean = values.sum.toDouble / values.size
  }
}
