package org.umass.ciir.feature
import java.io.{File, PrintWriter}

import org.umass.ciir.dataset.CommentAgree
import play.api.libs.json._
import org.umass.ciir.miscLib._
import org.umass.ciir.performance.TimeExpector

import scala.io.Source.fromFile

class CommentAnalyzer(dirPath : String) {
  val posCounter = new POSCounter(POSOption.Restricted)
  val commentAgree = new CommentAgree("C:\\work\\Data\\guardian data\\comment_agree_big.txt")

  def getAgreeVector(shortId:String) : List[Double] = {
    val pAgree = commentAgree.agreePortion(shortId)
    val pDisagree = commentAgree.disagreePortion(shortId)
    assert(pAgree + pDisagree < 1.0001)
    List(pAgree, pDisagree)
  }

  def getVector(shortId:String, comments : IndexedSeq[(Int, String, Option[Int])]) : List[Double]= {
    val targetComments = (comments map (_._2))
    val strDump = targetComments.mkString("\n")

    val vAgree : List[Double] = getAgreeVector(shortId) // 2
    val vPos: List[Double] = posCounter.getVector(strDump) // 6
    val vWords: List[Double] = WordFeature.getVector(strDump) // 4
    val vOverlap : List[Double] = OverlapFeature.getVector(comments)
    val vAvgLength: List[Double] = LengthFeature.getVector(targetComments.toList) // 1

    // 2 + 4 + 6 + 1 + 1
    val doubleV: List[Double] = vAgree ::: vWords ::: vPos ::: vOverlap ::: vAvgLength
    doubleV
  }

  val fout = new PrintWriter(new File("ArticleFeatureVector.txt"))
  val articleFiles = getListOfFiles(dirPath)
  println("Total of %d article (comment files)".format(articleFiles.length))
  val timer = new TimeExpector(articleFiles.length)
  articleFiles foreach {
    f => {
      timer.tick()
      val jsonPath = f.getAbsolutePath()
      val shortId : String = f.getName.split('.')(0).replace('_','/')
      val o = loadComments(jsonPath, shortId)
      o foreach { comments =>
          val v : List[Double] = getVector(shortId, comments)
          val outLine=  shortId + "\t" + v.mkString("\t") + "\n"
          fout.write(outLine)
      }
    }
  }
  fout.close()

  def loadComments(jsonPath : String, shortId : String) : Option[IndexedSeq[(Int, String, Option[Int])]] = {
    val rawJson: String = fromFile(jsonPath, "utf-8").mkString("")//.replace("\n","")
    val jsonObj = Json.parse(rawJson)
    val discussion = (jsonObj \ "discussion")
    val discussionId = (discussion \"key").asOpt[String].get
    assert(shortId == discussionId)
    val webUrl = (discussion \"webUrl").asOpt[String].get
    val commentCount = (discussion \"commentCount").asOpt[Int].get
    if(commentCount == 0)
      None
    else{
      val comments : Option[JsArray] = (discussion \ "comments").asOpt[JsArray]
      def parseComment(node : JsValue) = {
        val id = (node \ "id").asOpt[Int].get
        val body = (node \ "body").asOpt[String].get
        val discussion = (node \ "responses").asOpt[JsArray]
        def parseDiscussion(response: JsArray) : List[(Int, String, Option[Int])] = {
          response.value.toList map { v =>
            val body = (v \ "body").asOpt[String].get
            val id = (v \ "id").asOpt[Int].get
            val target = (v \ "responseTo" \ "commentId").asOpt[String].get.toInt
            (id, body, Some(target))
          }
        }
        (id, body, None) :: (discussion map parseDiscussion).getOrElse(List())
      }
      val commentList = comments map {
        x => x.value flatMap parseComment
      }
      commentList
    }
  }

}
