package org.umass.ciir.query

import org.umass.ciir.feature._
import org.umass.ciir.feature.TokenizerForTopic


class BM25(tokenizer : Tokenizer) {
  class Query(val query:List[String]){
    val tokens = tokenizer.stemNormalize(query)
    val queryVector : Map[String,Int] = tokens.groupBy(x =>x).mapValues(_.size)
  }
  val k1 = 1.2
  val k2 = 10
  val b = 0.75
  val avgDocLen = 1000
  def score(query: List[String] , doc :Document) : Double = {
    score(new Query(query), doc)
  }
  def score(query: Query, doc :Document) : Double = {
    val K = k1 * ((1-b) + b * doc.len / avgDocLen)
    def termWeight(term : (String, Int)) : Double = {
      val tf = doc.tf(term._1)
      val docSide = tf * (1+k1) / (tf + k1 * K)
      val querySide = term._2 * (1+k2) / (term._2 + k2)
      math.log(docSide * querySide)
    }
    (query.queryVector map termWeight).sum
  }
}
