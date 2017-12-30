package org.umass.ciir.query
import org.umass.ciir.dataset.GuardianDataSet.ArticleStructure

import scala.io.Source.fromFile
import org.umass.ciir.miscLib._

class Explain( scorePath : String, relPath : String) {

  def parseLine(line:String) : (List[String], Double) = {
    val tokens = line.split("\t")
    val query = tokens(0).split(" ").toList
    val score = tokens(1).toDouble
    (query, score)
  }
  val features = (fromFile(scorePath).getLines() map parseLine).toMap

  def parseRel(line:String) : (String, (List[String], Double)) = {
    val tokens = line.split("\t")
    val id = tokens(0)
    val query = tokens(1).split(" ").toList
    val score = tokens(2).toDouble
    (id, (query, score))
  }
  val relavance = {
    val rawList = fromFile(relPath).getLines() map parseRel
    rawList.toList.groupBy(l => l._1) mapValues (l => l map (_._2))
  }

  def asPhrase( article : ArticleStructure) : List[List[String]] = {
    val id = article.id.replace("/","_")
    val articleRelevance : List[(List[String], Double)] = relavance(id)
    def contribution( query_score:(List[String],Double) ) : Double = {
      features(query_score._1) * query_score._2
    }
    topKBy(articleRelevance,10)(contribution) map (_._1) map (_._1)
  }
}
