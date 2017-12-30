package org.umass.ciir.query

import edu.umass.ciir.ResourcePath
import org.umass.ciir.dataset.GuardianDataSet.ArticleStructure
import org.umass.ciir.feature.Tokenizer
import org.umass.ciir.miscLib._
import org.umass.ciir.performance.TimeExpector

import scala.io.Source.fromFile

class RM3( queryList : List[List[String]], articles : List[ArticleStructure], tokenizer:Tokenizer) {
  val stopwords = (fromFile(ResourcePath.stopwords).getLines() map (_.trim)).toSet

  val extQuery : Map[List[String], Map[String,Double]]= {
    val bm25 = new BM25(tokenizer)
    val corpus = articles map ( new Document(_, tokenizer))
    val globalCount = count(corpus flatMap (_.tokens))
    val C = globalCount.right sum
    def globalProb(str: String) : Double = globalCount(str).toDouble / C
    val mu = 400.0
    val timer = new TimeExpector(queryList.size)

    def expand(query : List[String]) : Map[String, Double] = {
      def scoreFn(doc: Document) = math.exp(bm25.score(query,doc))
      val topDocs : List[(Document, Double)] = (topKBy(corpus,10)(scoreFn))
      val allWords = (topDocs.left flatMap(_.tokens)).toSet.filterNot(stopwords(_)).toList
      val scoreSum = topDocs.right.sum
      def wordsWeight(words:List[String])(docAndScore:(Document, Double) ) : List[(String,Double)] = {
        val doc = docAndScore._1
        val docLen = doc.tokens.size
        val score = docAndScore._2
        val rawCount: Map[String,Int] = count(doc.tokens.toList)
        def wordProb(word:String) : Double = {
          val tf = rawCount.getOrElse(word,0)
          score * (tf + mu * globalProb(word)) / (docLen + mu)
        }
        words map {w => (w,wordProb(w))}
      }

      val init  = Map[String,Double]()
      def doubleSum( iterable: Iterable[Double]) : Double = iterable.sum
      val rawExpandedQuery : Map[String,Double] = groupMerge(topDocs flatMap wordsWeight(allWords), doubleSum)
      val terms = rawExpandedQuery.keys.toList
      topKBy(terms, 100)(rawExpandedQuery).toMap mapValues(_/scoreSum)
    }
    (queryList.par.map { x: List[String] => (x,expand(x)) }).toList.toMap
  }
}
