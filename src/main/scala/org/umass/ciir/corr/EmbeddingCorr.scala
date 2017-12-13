package org.umass.ciir.corr

import org.insight.wordspace.W2vSpace
import org.jblas.FloatMatrix

class EmbeddingCorr {
  println("Loading w2v..")
  val w2v = W2vSpace.load("C:\\work\\Data\\GoogleNews-vectors-negative300.bin\\GoogleNews-vectors-negative300.bin")


  def simpleTokenize(input: String) : Array[String] = input.split("[\\s\\.\\,\\?]")
  def getVect(token: String) : Option[FloatMatrix] = {
    if(w2v.contains(token))
      Some(w2v.vector(token))
    else {
      print("[%s]\n".format(token))
      None
    }
  }
  def sent2embedding(sentence : String): Array[FloatMatrix] = {
    val tokens = simpleTokenize(sentence)
    (tokens map getVect).flatten
  }

  def sum(embeddings : Iterable[FloatMatrix]): FloatMatrix = {
    def addFM(a : FloatMatrix, b: FloatMatrix) : FloatMatrix = a.add(b)
    embeddings.tail.foldRight(embeddings.head)(addFM)
  }


  def predict(sourceComment: String, candidate: List[String]) : List[Double] = {
    val srcEmbedding = sent2embedding(sourceComment)

    val trgEmbeddings = candidate map sent2embedding

    def similarity(src : Array[FloatMatrix])(trg : Array[FloatMatrix] ) : Double = {
      w2v.cosineSimilarity(sum(src), sum(trg))
    }
    trgEmbeddings map similarity(srcEmbedding)
  }

  def predict(sourceComments: List[String], candidate: List[String]) : List[Double] = {
    val scores = sourceComments map (predict(_, candidate))
    def addList(a:List[Double], b:List[Double]) : List[Double] = {
      (a zip b) map (p => p._1 + p._2)
    }
    scores.tail.foldRight(scores.head)(addList)
  }

  def topTokens(sourceComments: List[String], candidate: List[String]) : List[String] = {
    val articleTokens = (candidate map simpleTokenize flatten).toSet.toList
    val targets = articleTokens map getVect
    val srcEmb = sum((sourceComments map sent2embedding).flatten)
    val scores : List[Option[Double]] = targets map ( x => x match  {
      case Some(e) => Some(w2v.cosineSimilarity(srcEmb, e))
      case None => None
    })
    val scorePair = (articleTokens zip scores) flatMap {
      x => x._2 match {
        case Some(score) => Some(x._1, score)
        case none => None
      }
    }
    scorePair.sortBy(_._2).slice(0,10) map (_._1)
  }
}
