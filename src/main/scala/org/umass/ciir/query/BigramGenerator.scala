package org.umass.ciir.query

import edu.umass.ciir.ResourcePath
import org.umass.ciir.dataset.GuardianDataSet.ArticleStructure
import org.umass.ciir.feature.Tokenizer

import scala.io.Source.fromFile
import org.umass.ciir.miscLib._

class BigramGenerator(tokenizer: Tokenizer) {
  // 1. simple bigram
  // 2. windows 2 word
  val stopwords = (fromFile(ResourcePath.stopwords).getLines() map (_.trim)).toSet
  type Bigram = List[String]
  val windowSize = 5 // window is not used
  def gen(articles: List[ArticleStructure]) : Iterable[Bigram] = {
    val tokenArticles : List[Iterable[String]] = (articles map { a =>
      tokenizer.TokenizerNormalizeStemmer(a.title + "\n" + a.article)
    })
    val vocaCount = count(tokenArticles.flatten filterNot (stopwords.contains(_)))
    val corpusSize = vocaCount.values.sum

    def candidate( article : Iterable[String]) : List[Bigram] = {
      val seq = article.toIndexedSeq
      (for(
        i <- 0 until seq.length-1
        if( !stopwords.contains(seq(i)) && !stopwords.contains(seq(i+1)))
      ) yield List(seq(i), seq(i+1))).toList
    }
    val allBigrams = tokenArticles flatMap candidate
    val bigramCount = count(allBigrams)
    val candidateBigram = bigramCount.keys
    println("Total bigram : %d words : %d".format(vocaCount.keys.size, candidateBigram.size))

    def closeRelated( bigram: Bigram) : Boolean = {
      val a = bigram.head
      val b = bigram.tail.head
      // P(b|a)
      val p_b_bar_a = bigramCount(bigram).toDouble / vocaCount(a)
      // P(B)
      val p_b = vocaCount(b).toDouble / corpusSize
      p_b_bar_a > p_b * 2
    }
    candidateBigram filter closeRelated
    //
  }
}
