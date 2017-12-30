package org.umass.ciir.query

import opennlp.tools.stemmer.Stemmer
import org.umass.ciir.dataset.GuardianDataSet.ArticleStructure
import org.umass.ciir.feature.{Tokenizer, TokenizerForTopic}


class Document(article : ArticleStructure, tokenizer : Tokenizer) {
  val textDump = article.title + "\n" + article.article
  val tokens = tokenizer.TokenizerNormalizeStemmer(textDump)
  val sparseVector : Map[String,Int] = tokens.groupBy(x =>x).mapValues(_.size)


  def tf(word : String) = {
    if(sparseVector.contains(word)) sparseVector(word)
    else 0
  }
  val len : Int = tokens.size
}
