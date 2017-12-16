package org.umass.ciir.feature
import scala.io.Source._

class CommentTopic ( topicWordPath : String, commentTopicPath : String, maxTopic : Int){
  val wordPerTopic : Map[Int, Map[String, Double]]= {
    def parseLine(line: String ) : (Int, Map[String, Double]) ={
      val tokens = line.split("\t")
      val topicId = tokens(0).toInt
      def parseProb(token : String) : (String, Double) = {
        val tokens = token.split(":")
        (tokens(0), tokens(1).toDouble)
      }
      val prob = (tokens.drop(1) map parseProb).toMap
      assert(topicId <= maxTopic)
      (topicId, prob)
    }
    (fromFile(topicWordPath).getLines() map parseLine).toMap
  }
  val topicForComment : Map[String, IndexedSeq[Double]] = {
    def parseLine(line: String) : (String, IndexedSeq[Double]) = {
      val tokens = line.split("\t")
      val id = tokens(0)
      val probs = tokens.drop(2) map (_.toDouble)
      assert(probs.size == maxTopic )
      probs foreach ( x => assert(x >= 0 && x <=1))
      (id, probs)
    }
    (fromFile(commentTopicPath).getLines() map parseLine).toMap
  }

  def probability(topicProb : IndexedSeq[Double], word: String) : Double = {
    val probs = for(
      i <- 0 until topicProb.size
    ) yield topicProb(i) * wordPerTopic(i)(word)
    probs.sum
  }


}
