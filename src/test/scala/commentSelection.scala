import java.io.{File, PrintWriter}

import org.scalatest.FunSuite
import org.umass.ciir.classifier.CICommentRanker
import org.umass.ciir.dataset
import org.umass.ciir.dataset.GuardianDataSet.{ArticleStructure, Tool}
import org.umass.ciir.feature.{FeatureGenerator}
import play.api.libs.json.{JsArray, JsObject, JsString}

class commentSelection extends FunSuite {

  test("Comment Selection"){
    val articles: List[ArticleStructure] = Tool.getAll()
    val featureGen = new FeatureGenerator()
    val ranker = new CICommentRanker(featureGen)
    val articleList = List(14,169, 222) map articles

    articleList foreach { x =>
      val topComments : List[String] = ranker.topK(x, 30)
      println("article : " + x.title)
      topComments foreach { c =>
        println("CIC : " + c)
      }
    }
  }

  test("Generate contrv comment for topic modeling")
  {
    val articles: List[ArticleStructure] = Tool.getAll()
    val featureGen = new FeatureGenerator()
    val ranker = new CICommentRanker(featureGen)


    var counter = 0
    val articlePath = "guardianCtrvArticles\\"
    val commentPath = "guardianCtrvComments\\"
    articles foreach { article =>
      val sents = Tool.splitArticle(article.article) map {
        s => JsObject(Seq("id" -> JsString(s)))
      }
      val jObj = JsObject(Seq(
        "title" -> JsString(article.title),
        "content" -> JsString(article.article),
        "date" -> JsString(article.date),
        "name" -> JsString(article.id),
        "sentences" -> JsArray(sents)
      )
      )
      if(!sents.isEmpty)
      {
        val f = new PrintWriter(new File(articlePath + article.id+ ".json"))
        f.write(jObj.toString())
        f.close()
        def valid(comment:Map[String,String]) : Boolean = {
          val text = comment("text")
          text.size > 20 && !text.contains("This comment was removed by a moderator")
        }
        val contrvCommentEx = ranker.topKEx(article, 1000).filter(valid).slice(0,50)

        if(contrvCommentEx.size < 50) {
          print(contrvCommentEx.size)
          print(" - ")
          println(article.id)
        }

        contrvCommentEx foreach { commentEx =>
          val jObj = JsObject(Seq(
            "title" -> JsString("dummy"),
            "content" -> JsString(commentEx("text")),
            "parent" -> JsString(article.id),
            "cdate" -> JsString(commentEx("timestamp")),
            "name" -> JsString(commentEx("id")),
          ))
          val f = new PrintWriter(new File(commentPath + commentEx("id") + ".json"))
          f.write(jObj.toString())
          f.close()
        }
      }
    }

  }

}
