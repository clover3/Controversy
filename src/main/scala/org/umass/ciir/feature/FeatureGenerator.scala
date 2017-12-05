package org.umass.ciir.feature

import org.umass.ciir.feature.GuardianDataSet.ArticleStructure
import org.umass.ciir.svm.SVMWrap.Instance

class FeatureGenerator {
  val posCounter = new POSCounter(POSOption.Restricted)

  def apply(x : ArticleStructure) : Instance = {
    val targetComments = x.comments
    val strDump = targetComments.mkString("\n")
    val vPos : List[Double] = posCounter.getVector(strDump)
    val vWords : List[Double] = WordFeature.getVector(strDump)
    val vLength : List[Double] = LengthFeature.getVector(targetComments)
    val vOverlap : List[Double] = OverlapFeature.getVector(x)
    val doubleV : List[Double] = vPos ::: vWords ::: vLength ::: vOverlap
    new Instance(doubleV)
  }
  def saveCache() = posCounter.saveCache()
}
