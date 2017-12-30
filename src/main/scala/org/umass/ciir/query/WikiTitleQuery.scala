package org.umass.ciir.query

import scala.io.Source.fromFile

class WikiTitleQuery {
  def loadFromMScore() : List[String] = {
    val lines = fromFile("C:\\work\\Data\\clueweb\\MScore.txt").getLines()
    def parseLine(line:String) : (Double, String) = {
      val tokens = line.split("\\s")
      (tokens(0).toDouble, tokens(1).trim)
    }
    val scoreTitleList : List[(Double, String)] = (lines map parseLine).toList

    scoreTitleList map (_._2)
  }

  def parseTitle(rawTitle: String) : List[String] ={
    rawTitle.split("[_,]").toList
  }

  lazy val parsedTitle = loadFromMScore() map parseTitle
}
