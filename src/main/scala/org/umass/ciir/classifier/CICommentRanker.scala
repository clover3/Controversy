package org.umass.ciir.classifier

import org.umass.ciir.dataset.GuardianDataSet._
import org.umass.ciir.feature.OverlapFeature.commonToken
import org.umass.ciir.feature.{CommentRegression, FeatureGenerator, OverlapFeature}

import scala.collection.mutable

class CICommentRanker(featureGen: FeatureGenerator)
{
  val commentRegression = new CommentRegression(featureGen)
  def topK(x: ArticleStructure, k : Int) : List[String] = {
    topKEx(x, k) map (_("text"))
  }
  def topKEx(x: ArticleStructure, k : Int) : List[Map[String, String]]= {

    val commentDict : Map[String, String] = {
      x.commentExs map { comment => comment("id") -> comment("text")}
    }.toMap

    val overlaps : mutable.Map[String, List[Double]] = mutable.Map[String, List[Double]]()
    def update(id : String, value : Double) = {
      val newList = overlaps.get(id) match {
        case Some(l) => value :: l
        case None => List(value)
      }
      overlaps.put(id, newList)
    }

    val q = x.commentExs map { comment =>
      val prevId = comment("reply-to")
      if(prevId.size > 1)
      {
        try {
          val post: String = commentDict(prevId)
          val reply: String = comment("text")
          val nCommon = commonToken(post, reply)
          val id = comment("id")
          update(id, nCommon)
          update(prevId, nCommon)
        } catch{
          case e:NoSuchElementException => {
          }
        }
      }
    }
    val overlapMap : mutable.Map[String, Double] = overlaps map { x =>
      val l : List[Double] = x._2
      (x._1, l.sum / l.size)
    }


    val overlapDef = 0.46

    val scores = x.commentExs map { comment =>
      val id = comment("id")
      val text: String = comment("text")
      val overlap = overlapMap.get(id) match {
        case Some(x) => x
        case None => overlapDef
      }
      commentRegression(featureGen.getVector(text, overlap).toArray)
    }

    val topComments = (x.commentExs zip scores).sortBy(_._2).slice(0,k)
    topComments map (_._1)
  }

}
