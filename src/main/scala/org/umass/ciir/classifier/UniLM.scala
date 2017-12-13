package org.umass.ciir.classifier

class UniLM(corpus : List[String]) {
  def tokenize(input: String) : Array[String] = input.split("[\\s\\.\\,\\?!]")
  val allTokens = corpus flatMap tokenize
}
