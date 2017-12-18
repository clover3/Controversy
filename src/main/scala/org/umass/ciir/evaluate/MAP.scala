package org.umass.ciir.evaluate

import org.umass.ciir.miscLib._

object MAP {
  def AP(at:Int , ranked : IndexedSeq[Int], relevance : Map[Int,Boolean]) : Double = {
    def precision(list :  IndexedSeq[Int]) : Option[Double] = {
      if(relevance(list.last) )
        Some((list count relevance).toDouble /  list.size)
      else
        None
    }
    def min(a:Int, b:Int) : Int = if(a>b) a else b
    val poArr : IndexedSeq[Option[Double]]= for(
      i <- 0 until min(ranked.size, at)
    ) yield precision(ranked.slice(0, i+1))
    val pArr = poArr.flatten
    if(pArr.isEmpty)
      0
    else
      average(pArr.toArray)
  }
}
