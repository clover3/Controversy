package org.umass.ciir.dataset
import org.umass.ciir.miscLib._
import scala.io.Source.fromFile

class CommentAgree(path : String) {
  val t0 = System.currentTimeMillis()

  type ShortID = String
  type ArticleID = String
  type CommentID = String
  def parseLine(line:String) : (ShortID,(CommentID, String)) = {
    val tokens = line.split("\t")
    val shortId = tokens(0).replace("_","/")
    val url = tokens(1)
    val idx = url.indexOf(".com/")
    val id = url.substring(idx+4)
    assert(id.charAt(0) == '/')
    val commentId = tokens(2)
    val agreeJudge = tokens(3)
    (shortId , (commentId, agreeJudge))
  }
  println("CommentAgree [Loading]")
  val data = (fromFile(path).getLines() map parseLine)

  val sAgree = "1"
  val sDisagree = "2"

  val dataMap : Map[ShortID, Map[CommentID, String]] = groupMerge(data.toList, {x : List[(String,String)]=> x.toMap})
  val agreePortion : Map[ShortID, Double] = dataMap.mapValues {
      m => m.values.count(_ == sAgree).toDouble / m.size
    }
  val disagreePortion : Map[ShortID, Double] = dataMap.mapValues {
    m => m.values.count(_ == sDisagree).toDouble / m.size
  }

  def has(articleID:ShortID, commentID: String) : Boolean = dataMap.contains(articleID) && dataMap(articleID).contains(commentID)
  def isAgree(articleID: ShortID, commentID:String) : Boolean = dataMap(articleID)(commentID) == "1"
  def isDisagree(articleID: ShortID, commentID:String) : Boolean = dataMap(articleID)(commentID) == "2"
  val t1 = System.currentTimeMillis()
  println("Total of %d articles".format(dataMap.keys.size))
  println("CommentAgree [Loaded] %d ms".format(t1-t0))
}
