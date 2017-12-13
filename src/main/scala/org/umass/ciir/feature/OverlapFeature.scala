package org.umass.ciir.feature

import org.umass.ciir.feature.GuardianDataSet.ArticleStructure

object OverlapFeature {
  def commonToken(str1: String, str2: String): Int = {
    val set1 = str1.split("\\s").toSet
    str2.split("\\s") count (set1(_))
  }
  def getVector(x : ArticleStructure) : List[Double] = {
    val commentDict : Map[String, String] = {
      x.commentExs map { comment => comment("id") -> comment("text")}
    }.toMap

    val commonTokens : List[Option[Int]] = x.commentExs map { comment =>
      val prevId = comment("reply-to")
      if(prevId.size > 1)
      {
        try {
          val post: String = commentDict(prevId)
          val reply: String = comment("text")
          Some(commonToken(post, reply))
        } catch{
          case e:NoSuchElementException => {
            None
          }
        }
      }
      else
        None
    }
    val validCommonTokens = commonTokens.flatten
    val avgCommon = {
      if (validCommonTokens.size > 0)
        validCommonTokens.sum.toDouble / validCommonTokens.size / 100
      else
        0
    }
    List(avgCommon)
  }
}
