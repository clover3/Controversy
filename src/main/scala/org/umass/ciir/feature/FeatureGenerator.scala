package org.umass.ciir.feature

import org.umass.ciir.dataset.GuardianDataSet._
import org.umass.ciir.svm.SVMWrap.Instance

class FeatureGenerator {
  val posCounter = new POSCounter(POSOption.Restricted)

  def getVector(x : ArticleStructure) : List[Double] = {
    val targetComments = x.comments
    val strDump = targetComments.mkString("\n")

    val vAgree: List[Double] = AgreeFeature.getVector(targetComments) // 2
    val vPos: List[Double] = posCounter.getVector(strDump) // 6
    val vWords: List[Double] = WordFeature.getVector(strDump) // 4
    val vLength: List[Double] = LengthFeature.getVector(targetComments) // 1
    val vOverlap: List[Double] = OverlapFeature.getVector(x) // 1
    // 2 + 4 + 6 + 1 + 1
    val doubleV: List[Double] = vPos ::: vWords ::: vLength ::: vOverlap//vAgree ::: vWords ::: vPos ::: vOverlap ::: vLength
    doubleV
  }
  def getVector(commentString : String, overlapV : Double) : List[Double] = {
    val strDump = commentString
    val vAgree: List[Double] = AgreeFeature.getVector(List(commentString)) // 2
    val vPos: List[Double] = posCounter.getVector(strDump) // 6
    val vWords: List[Double] = WordFeature.getVector(strDump) // 4
    val vLength: List[Double] = LengthFeature.getVector(List(commentString)) // 1
    val vOverlap: List[Double] = List(overlapV) // 1
    val doubleV: List[Double] = vAgree ::: vWords ::: vPos ::: vOverlap ::: vLength
    doubleV
  }

  def apply(x : ArticleStructure) : Instance = {
    new Instance(getVector(x))
  }
  def saveCache() = posCounter.saveCache()
}
