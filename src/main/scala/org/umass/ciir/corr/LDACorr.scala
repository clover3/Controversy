package org.umass.ciir.corr

import org.umass.ciir.classifier.UniLM
import org.umass.ciir.feature.LDAWrap

class LDACorr (corpus : List[String]) {
  val LDA = new LDAWrap(corpus, 100)
  val LM = new UniLM(corpus)

  def estimate(source: String, candidate: List[String]): IndexedSeq[Double] = {
    val ldaScores : IndexedSeq[Double] = LDA.estimate(source, candidate)
    ???
  }
}