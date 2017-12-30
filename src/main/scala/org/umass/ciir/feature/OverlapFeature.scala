package org.umass.ciir.feature

import org.umass.ciir.dataset.GuardianDataSet._

object OverlapFeature {
  def commonToken(str1: String, str2: String): Int = {
    val set1 = str1.split("\\s").toSet
    str2.split("\\s") count (set1(_))
  }

  def countCommonTokens[A](text:String, optPrevId : Option[A], dict: Map[A,String]): Option[Int] ={
    optPrevId match {
      case Some(prevId) => {
        try {
          val post: String = dict(prevId)
          val reply: String = text
          Some(commonToken(post, reply))
        } catch {
          case e: NoSuchElementException => None
        }
      }
      case None => None
    }
  }

  private def getAverage(validCommonTokens: List[Int]) = {
    val avgCommon = {
      if (validCommonTokens.size > 0)
        validCommonTokens.sum.toDouble / validCommonTokens.size / 100
      else
        0
    }
    List(avgCommon)
  }

  def getVector(x : ArticleStructure) : List[Double] = {
    val commentDict : Map[String, String] = {
      x.commentExs map { comment => comment("id") -> comment("text")}
    }.toMap
    val commonTokens : List[Option[Int]] = x.commentExs map { comment =>
      val text = comment("text")
      val optPrevId = if(comment("reply-to").size > 0 ) Some(comment("reply-to")) else None
      countCommonTokens(text, optPrevId, commentDict)
    }
    val validCommonTokens = commonTokens.flatten
    getAverage(validCommonTokens)
  }


  def getVector(comments : IndexedSeq[(Int, String, Option[Int])])= {
    val commentDict : Map[Int, String] = {
      comments map { comment => comment._1 -> comment._2 }
    }.toMap
    val commonTokens : IndexedSeq[Option[Int]] = comments map (c => countCommonTokens(c._2, c._3, commentDict) )
    val validCommonTokens = commonTokens.flatten
    getAverage(validCommonTokens.toList)
  }
}
