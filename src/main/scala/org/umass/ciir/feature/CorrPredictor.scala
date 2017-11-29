package org.umass.ciir.feature

import org.umass.ciir.feature.GuardianDataSet.ArticleStructure

class CorrPredictor {
  val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
  private def extract( a : ArticleStructure) : List[String] = {
    a.article :: a.comments.slice(0,10)
  }
  private val corpus = (articles map extract).flatten
  private val lda = new LDAWrap(corpus)


  def predict(sourceComment: String, candidate: List[String]) : String = {
    //lda.estimate()
    ???
  }

}
