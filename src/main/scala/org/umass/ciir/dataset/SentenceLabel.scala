package org.umass.ciir.dataset

import java.io.File

import org.umass.ciir.evaluate.MAP
import org.umass.ciir.miscLib._

import scala.io.Source.fromFile
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

class SentenceLabel(dirPath : String) {
  val threshold = 4

  class Article(path : String, val id : String) {
    val data : Array[(Int, String)]  = (fromFile(path).getLines() map { line =>
      val tokens = line.split("\t")
      (tokens(1).toInt, tokens(2))
    }).toArray
    def apply(idx : Int): (Int, String)= {
      data(idx)
    }
    def getRelevance(threshold : Int) : Map[Int, Boolean] = {
      {for(
        i <- 0 until data.size
      ) yield (i-> (data(i)._1 >= threshold))}.toMap
    }
    def sentences() : Array[String] = data map (_._2)
  }
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
  val files :List[File]= getListOfFiles(dirPath)
  val articles : List[Article] = files map {f =>
    val id = f.getName.slice(0, f.getName.indexOf(".txt"))
    new Article(f.getAbsolutePath(), id)
  }

  def getAP(at:Int, article:Article, scoreFunction : (String => Double)) : Double = {
    val res = for{
      i <- 0 until article.data.size
    } yield (i, scoreFunction(article.data(i)._2))
    val rank = res.sortBy(- _._2) map (_._1)
    MAP.AP(at, rank, article.getRelevance(threshold))
  }

  def getMAP(at :Int, scoreFunction : (String=>Double) ) : Double = {
    val arrayAP = articles map {getAP(at, _, scoreFunction)}
    arrayAP foreach println
    average(arrayAP)
  }

  def getMAP2(at :Int, scoreFunction : (String => String=>Double) ) : Double = {
    val arrayAP = articles map {
      article => getAP(at, article, scoreFunction(article.id))
    }
    arrayAP foreach println
    average(arrayAP)
  }
}
