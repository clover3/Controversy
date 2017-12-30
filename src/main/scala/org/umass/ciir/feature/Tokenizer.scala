package org.umass.ciir.feature

trait Tokenizer {
  def stemNormalize(words : List[String]) : List[String]
  def TokenizerNormalizeStemmer(source: String): Iterable[String]
}
