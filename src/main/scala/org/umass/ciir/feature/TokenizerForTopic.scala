package org.umass.ciir.feature

import java.io.FileInputStream
import java.text.Normalizer
import java.util
import java.util.LinkedList

import scala.io.Source.fromFile

import edu.umass.ciir.ResourcePath
import org.tartarus.snowball.SnowballStemmer
import org.tartarus.snowball.ext.englishStemmer
import opennlp.tools.tokenize.{Tokenizer, TokenizerME, TokenizerModel}

class TokenizerForTopic extends Tokenizer {
  val tokenizer = new TokenizerME(new TokenizerModel(new FileInputStream(ResourcePath.tokenModel)))
  val stemmer = new englishStemmer
  val stopwords : Set[String] = {
    val tokens = fromFile(ResourcePath.stopwords).getLines() map ( s => SnowballStemming(Normalize(s)))
    tokens.filterNot(_.isEmpty).toSet
  }

  protected def SnowballStemming(token: String): String = {
    stemmer.setCurrent(token)
    if (stemmer.stem) stemmer.getCurrent
    else token
  }

  protected def isLegit(token: String): Boolean = !token.isEmpty && !stopwords.contains(token) && token.length > 1 && token.length < 20

  protected def isNumber(token: String): Boolean = token.matches("\\d+")

  protected def Normalize(token: String): String = {
    val token1 = Normalizer.normalize(token, Normalizer.Form.NFKC)
    val token2 = token.replaceAll("\\W+", "")
    val token3 = token.toLowerCase
    if (isNumber(token3)) "NUM"
    else token3
  }

  def stemNormalize(words : List[String]) : List[String] =
    words map (s => SnowballStemming(Normalize(s)))


  def TokenizerNormalizeStemmer(source: String): Iterable[String] = {
    val rawTokens = tokenizer.tokenize(source)
    val tokens = rawTokens map (s => SnowballStemming(Normalize(s)))
    tokens filter isLegit
  }
}
