package org.umass.ciir.feature

import java.io.FileInputStream

import edu.umass.ciir.ResourcePath
import opennlp.tools.tokenize.{TokenizerME, TokenizerModel}
import org.lemurproject.kstem.KrovetzStemmer

class SimpleTokenzier extends Tokenizer {

  def stemNormalize(words : List[String]) : List[String] = words map (_.replaceAll("[^\\x00-\\x7F]", "")) map stemmer.stem

  val stemmer = new KrovetzStemmer


  val tokenizer = new TokenizerME(new TokenizerModel(new FileInputStream(ResourcePath.tokenModel)))
  override
  def TokenizerNormalizeStemmer(source: String): Iterable[String] = {
    val asciiString = source.replaceAll("[^\\x00-\\x7F]", "")
    val tokens = tokenizer.tokenize(asciiString)
    stemNormalize(tokens.toList)
  }
}
