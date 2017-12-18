package org.umass.ciir.feature
import scala.io.Source._

class NewsTopic(topicWordPath : String, commentTopicPath : String, parentTopicPath : String, maxTopic : Int){
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
  val voca = wordPerTopic.head._2.keySet
  def loadTopic(path : String) = {
    def parseLine(line: String) : (String, Array[Double]) = {
      val tokens = line.split("\t")
      val id = tokens(0)
      val probs = tokens.slice(2, 2+maxTopic) map (_.toDouble)
      assert(probs.size == maxTopic )
      probs foreach ( x => assert(x >= 0 && x <=1))
      (id, probs)
    }
    (fromFile(path).getLines() map parseLine).toMap
  }
  val topicForComment : Map[String, Array[Double]] = loadTopic(commentTopicPath)
  val topicForArticle : Map[String, Array[Double]] = loadTopic(parentTopicPath)
  val commentIDs = topicForComment.keySet

  def probability(topicProb : IndexedSeq[Double])(word: String) : Option[Double] = {
    try {
      val probs = for (
        i <- 0 until topicProb.size
      ) yield topicProb(i) * wordPerTopic(i)(word)
      Some(probs.sum)
    }catch {
      case e: NoSuchElementException => None
    }
  }
}
