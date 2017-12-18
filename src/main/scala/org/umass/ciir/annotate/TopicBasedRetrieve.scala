package org.umass.ciir.annotate


import org.umass.ciir.dataset.GuardianDataSet._

import scala.collection.mutable
import org.umass.ciir.feature.{NewsTopic, Tokenizer}
import org.umass.ciir.dataset._
import org.umass.ciir.miscLib._

class TopicBasedRetrieve {
  val nTopic = 200
  val topicPath = "TopicData\\Run2\\"
  val pathTopicWord = topicPath + "topicWord.txt"
  val pathChildParam = topicPath + "childParameter.txt"
  val pathParentParam = topicPath + "parentParameter.txt"
  val newsTopic = new NewsTopic(pathTopicWord, pathChildParam, pathParentParam, nTopic)
  val titleList : List[String] = new ControversyList("resource\\controversyList.txt").list.toList
  val tokenizer = new Tokenizer()
  val titleTokens : Map[String, Array[String]] = (titleList map { x => (x,tokenizer.TokenizerNormalizeStemmer(x).toArray) }).toMap
  val titleMax = 6
  val bgProb = 1E-5
  val baseline = product(Array.fill(titleMax)(bgProb))
  assert(baseline > 0)
  assert(baseline < 0.00001)

  val missingWords : mutable.Set[String] = mutable.Set[String]()

  def product(arr: Iterable[Double]): Double = arr.foldRight(1.0){(x:Double,y:Double) => x * y }

  def avg(l : Array[Double]) : Double = l.sum / l.size
  // given set of comments pick the most probable topic,
  def pickTitle(commentIDs : List[String]) : Option[String] = {
    val (title, score) = titleTopK(commentIDs,1).head
    if(score > baseline)
      Some(title)
    else
      None
  }


  def titleTopK(commentIDs : List[String], k : Int) : List[(String, Double)] = {
    val topicVectors = commentIDs.toArray map newsTopic.topicForComment
    val avgTopic = topicVectors.transpose map ( l => avg(l) )
    assert(avgTopic.size == nTopic)
    val getProb = newsTopic.probability(avgTopic)(_)

    def score(title : String) : Double = {
      val tokens = titleTokens(title)

      val probArr = (0 until titleMax) map { i =>
        if (i < tokens.size ) {
          getProb(tokens(i)) match {
            case Some(p) => p
            case None =>{
              missingWords.add(tokens(i))
              bgProb
            }
          }
        }
        else
          bgProb
      }
      product(probArr)
    }
    topKBy(titleList, k)(score)
  }

  def topwords(text : String, articleID:String, commentIDs : List[String]) : List[String] = {
    val candidates = tokenizer.TokenizerNormalizeStemmer(text).toSet.toList
    topwords(candidates, articleID, commentIDs)
  }

  def topwords(candidateVoca : List[String], articleID:String, commentIDs : List[String]) : List[String] = {
    val articleTopic = newsTopic.topicForArticle(articleID)
    val topicVectors = commentIDs.toArray map newsTopic.topicForComment
    val avgTopic = topicVectors.transpose map ( l => avg(l) )
    def diff(word:String) : Option[Double] = {
      val commentP = newsTopic.probability(avgTopic)(word)
      val articleP = newsTopic.probability(articleTopic)(word)
      if( commentP.isDefined && articleP.isDefined)
        Some(commentP.get)// - articleP.get)
      else
        None
    }
    val r = topKBy(candidateVoca,10)(diff)
    r map (_._1)
  }

}

class TopicBasedRetrieveEx extends TopicBasedRetrieve{
  val articles : Map[String, ArticleStructure] = (Tool.getAll() map { article =>
    (article.id -> article)
  }).toMap

  def tokenize(str:String) = {
    val textAscii = str.replaceAll( "[^\\x00-\\x7F]", "" )
    tokenizer.TokenizerNormalizeStemmer(textAscii).toArray
  }

  def logProductAvg(arr: Iterable[Double]) : Double = {
    average(arr map math.log)
  }

  def articleRater(articleID: String)(text:String) : Double = {
    def exists(commentID: String) : Boolean = {
      newsTopic.commentIDs.contains(commentID)
    }
    val commentIds = articles(articleID).commentExs map (_("id"))

    val pickedComments = commentIds filter exists
    val articleTopic = newsTopic.topicForArticle(articleID)
    val bgProb = {
      val tokens = tokenize(articles(articleID).article)
      val probs = (tokens map (newsTopic.probability(articleTopic)(_))).flatten
      val bgProb = avg(probs)
      bgProb
    }

    val topicVectors = pickedComments.toArray map newsTopic.topicForComment
    val avgTopic = topicVectors.transpose map ( l => avg(l) )
    val getProb = newsTopic.probability(avgTopic)(_)
    val maxLength = 50
    def score(text : String) : Double = {
      val tokens = tokenize(text)
      val probArr = (tokens map getProb).flatten
      logProductAvg(probArr)
    }
    val s= score(text)
    s
  }
}