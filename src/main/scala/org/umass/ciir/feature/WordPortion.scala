package org.umass.ciir.feature

import scala.io.Source


object WordFeature{

  trait PortionBase {
    def portion(input: String): Double
    def spaceTokenize(input: String) : Array[String] = input.split("\\s")
    def numTokens(input: String) : Int = spaceTokenize(input).size
  }

  object QuotationPortion extends PortionBase {
    def portion(input:String) : Double ={
      input
      val chars = List('\'','\"')
      val occurrence :Int = (chars map {quote => input.count(_ == quote)}).sum
      occurrence.toDouble / numTokens(input)
    }
  }

  class WordPortion(listPath: String, debug: Boolean = false) extends PortionBase {
    val words = Source.fromFile(listPath).getLines.toSet

    def portion(input: String): Double = {
      val tokens = spaceTokenize(input)
      val p = (tokens count (x => words.contains(x))).toDouble / tokens.size
      if (debug) print("%f ".format(p))
      p
    }
  }

  val offensiveFeature = new WordPortion("resource\\bad-words.txt")
  val agreeFeature = new WordPortion("resource\\agreement.txt")
  val disagreeFeature = new WordPortion("resource\\disagreement.txt")
  val featureList = List(offensiveFeature, QuotationPortion, agreeFeature, disagreeFeature)

  def getVector(input: String): List[Double] = {
    featureList map (x => x.portion(input))
  }
}