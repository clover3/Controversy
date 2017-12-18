package org.umass.ciir.annotate

import org.umass.ciir.miscLib._

import scala.io.Source.fromFile

class LMRetrieve( path : String) {

  val scoreMap : Map[String, Double] =  {
    def parseLine(line:String) : (String, Double) = {
      val tokens = line.split("\t")
      (tokens(0), tokens(1).toDouble)
    }
    (fromFile(path).getLines() map parseLine).toMap
  }
  def tokenize(sentence:String) : List[String] = sentence.split("[\\s\\.\\,\\?]").toList

  def score(word:String) : Double = {
    if(scoreMap.contains(word))
      scoreMap(word)
    else
      0
  }

  def topwords(text : String) : List[String] = {
    val candidates : List[String] = tokenize(text)
    topwords(candidates)
  }

  def topwords(candidateVoca : List[String]) : List[String] = {
    val r = topKBy(candidateVoca, 10)(score)
    r map (_._1)
  }

  def rateSentence(s : String) : Double = {
    (tokenize(s) map score).sum
  }


}
