package org.umass.ciir.feature

import scala.io.Source

object AgreeFeature {
  val lines = Source.fromFile("resource\\commentAgree.txt").getLines.toList map (_.replace("\n","").replace("\r",""))

  def parseLine(line: String) : (String, Int) = {
    (line.substring(2), line.substring(0,1).toInt)
  }
  val agreeClassifier = (lines map parseLine).toMap

  def getAgree(comment: String) : Int = {
    val key = comment.replace("\n"," ").replace("\r"," ")
    agreeClassifier(key)
  }

  def getVector(comments : List[String]) : List[Double] = {
    val labels = comments map getAgree
    val total = comments.size
    val perAgree = labels.count(_==1).toDouble
    val perDisagree = labels.count(_==2).toDouble
    List(perAgree/total, perDisagree/total)
  }
}
