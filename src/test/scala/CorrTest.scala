import org.scalatest.FunSuite
import org.umass.ciir.feature.{CorrPredictor, GuardianDataSet, LDAWrap}
import org.umass.ciir.feature.GuardianDataSet.ArticleStructure

class CorrTest extends FunSuite {

  test("BaseTest")
  {
    val article : Seq[ArticleStructure] = GuardianDataSet.Tool.getAll().toSeq
    val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    def extract( a : ArticleStructure) : List[String] = {
      a.article :: a.comments.slice(0,10)
    }
    val corpus = (articles map extract).flatten
    val lda = new LDAWrap(corpus)
    article.slice(1,10) foreach { x =>
      val comment = x.comments.head
      val articleParts = GuardianDataSet.Tool.splitArticle(x.article)
      val rel = lda.estimate(comment, articleParts)
      println("Comment : " + comment)

      (articleParts zip rel) foreach (pair => println("article : (" + pair._2 + ") " +  pair._1))

    }
  }

  test("CorrAndWrite"){

    val article : Seq[ArticleStructure] = GuardianDataSet.Tool.getAll().toSeq

    val predictor = new CorrPredictor()

    article.slice(1,10) foreach { x =>
      val comment = x.comments.head
      val articleParts = GuardianDataSet.Tool.splitArticle(x.article)
      val mostRel = predictor.predict(comment, articleParts)
      println("Comment : ", comment)
      articleParts foreach (para => println("article : ", para))
      println("Answer : ", mostRel)
    }
  }
}
