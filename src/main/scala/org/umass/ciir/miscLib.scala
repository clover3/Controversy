package org.umass.ciir

import java.io.File

object miscLib {
  def time[R](block: => R, msg: String = ""): R = {
    print(msg + "\t")
    Console.flush()
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


  def count[A]( l : List[A]) : Map[A,Int] = l.groupBy(x=>x).mapValues( x => x.size)
  implicit class ImplMap[A,B](values : Map[A,B]) {
    def left : Iterable[A] = values map { case (a,b) => a}
    def right : Iterable[B] = values map { case (a,b) => b }
  }

  implicit class ImplListPair[A,B](values : List[(A,B)]) {
    def left : List[A] = values map (_._1)
    def right : List[B] = values map (_._2)
  }

  def groupMerge[A,B,C]( l : List[(A,B)], reducer : List[B]=>C) : Map[A,C] = {
    l.groupBy(_._1).mapValues(_.map(_._2)) mapValues reducer
  }

  def groupMerge2[A,B,C]( l : List[(A,B)], reducer : Iterable[(A,B)]=>C) : Map[A,C] = {
    l.groupBy(_._1).mapValues(reducer)
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
}
