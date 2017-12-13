package org.umass.ciir.feature

import scala.io.Source


object WordFeature{

  trait PortionBase {
    def portion(input: String): Double
    def simpleTokenize(input: String) : Array[String] = input.split("[\\s\\.\\,\\?]")
    def numTokens(input: String) : Int = simpleTokenize(input).size
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
    if(debug)
      words foreach println

    def portion(input: String): Double = {
      val tokens = simpleTokenize(input)
      val falseTokens = tokens filterNot(words.contains(_))

      val p = (tokens count (x => words.contains(x))).toDouble / tokens.size
      if (false) {
        falseTokens foreach { t =>
          print("%s ".format(t))
        }
        println()
      }
      p
    }
  }

  val offensiveFeature = new WordPortion("resource\\bad-words.txt")
  val agreeFeature = new WordPortion("resource\\agreement.txt")
  val disagreeFeature = new WordPortion("resource\\disagreement.txt", false)
  val featureList = List(offensiveFeature, QuotationPortion, agreeFeature, disagreeFeature)

  def getVector(input: String): List[Double] = {
    featureList map (x => x.portion(input))
  }
}