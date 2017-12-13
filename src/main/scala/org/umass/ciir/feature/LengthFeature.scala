package org.umass.ciir.feature

object LengthFeature {

  def getVector(comments : List[String] ) : List[Double] = {
    def countTokens(input: String): Int = {
      val tokens = input.split("\\s")
      tokens.size
    }
    val arr = comments map countTokens
    val mean = arr.sum.toDouble/ arr.size / 100
    List(mean)
  }
}
