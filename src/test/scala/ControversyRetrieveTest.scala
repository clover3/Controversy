import org.scalatest.FunSuite
import org.umass.ciir.annotate.{LMRetrieve, TopicBasedRetrieve, TopicBasedRetrieveEx}
import org.umass.ciir.feature.NewsTopic
import org.umass.ciir.dataset.GuardianDataSet._
import org.umass.ciir.dataset.SentenceLabel
import org.umass.ciir.miscLib._
import org.umass.ciir.evaluate.MAP

import scala.util.Random

class ControversyRetrieveTest extends FunSuite{
  test("CommentTopic test") {
    val commentTopic = new NewsTopic("topicWord.txt", "childParameter.txt", "parentParameter.txt", 200)
    commentTopic.wordPerTopic(10) foreach println
  }

  test("One Test"){
    val retriever = new TopicBasedRetrieve
    def exists(commentID: String) : Boolean = {
      retriever.newsTopic.commentIDs.contains(commentID)
    }
    Tool.getAll() foreach { article =>
      val commentIds = article.commentExs map (_("id"))
      val pickedComments = commentIds filter exists

      if( !pickedComments.isEmpty){
        val res = retriever.pickTitle(pickedComments)
        if( res.isDefined)
        {
          print(article.title)
          print("\t%d\t".format(pickedComments.size))
          println(res.get)
        }
      }
    }
  }

  test("Top K "){
    val retriever = new TopicBasedRetrieve
    def exists(commentID: String) : Boolean = {
      retriever.newsTopic.commentIDs.contains(commentID)
    }
    val k = 4
    Tool.getAll() foreach { article =>
      val commentIds = article.commentExs map (_("id"))
      val pickedComments = commentIds filter exists
      print(article.title)
      print("\t%d\t".format(pickedComments.size))
      if( !pickedComments.isEmpty)
      {
        retriever.titleTopK(pickedComments,k) foreach println
      }
    }
  }


  test("From article "){
    val retriever = new TopicBasedRetrieve
    def exists(commentID: String) : Boolean = {
      retriever.newsTopic.commentIDs.contains(commentID)
    }
      Tool.getAll() foreach { article =>
        val commentIds = article.commentExs map (_("id"))
        val pickedComments = commentIds filter exists
      if( pickedComments.size > 0)
      {
        print(article.title)
        print("-")
        println(pickedComments.size)
        retriever.topwords(article.article, article.id, pickedComments) foreach println
      }
    }
  }

  test("LM based keyword retrieve"){
    val retriever = new LMRetrieve("resource\\oddScores.txt")
    Tool.getAll() foreach { article =>
      {
        println(article.title)
        retriever.topwords(article.article) foreach println
      }
    }
  }

  test("Sentence Ranking"){
    val retriever = new LMRetrieve("resource\\oddScores.txt")
    val sl = new SentenceLabel("C:\\work\\Data\\guardian data\\Sentence Label")

    val randomRate:(String=>Double) = {x =>Random.nextDouble()}
    println("Random   : " + sl.getMAP(10,randomRate))

    val lmf : String=>Double = retriever.rateSentence
    println("LM Based : " + sl.getMAP(10, lmf))

    val tr = new TopicBasedRetrieveEx
    val tmf : String=>String=>Double = tr.articleRater
    println("TM Based : " + sl.getMAP2(10,tmf))

    tr.missingWords foreach { x => print(x + " ") }
  }
}
