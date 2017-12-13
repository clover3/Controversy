package corr

import org.scalatest.FunSuite
import org.umass.ciir.classifier.CICommentRanker
import org.umass.ciir.corr.EmbeddingCorr
import org.umass.ciir.feature.GuardianDataSet.ArticleStructure
import org.umass.ciir.feature.{FeatureGenerator, GuardianDataSet, LDAWrap}

class CorrTest extends FunSuite {

  test("LDA Base summary")
  {
    val article : Seq[ArticleStructure] = GuardianDataSet.Tool.getAll().toSeq
    val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    def extract( a : ArticleStructure) : List[String] = {
      List(a.article)
    }
    val corpus = (articles map extract).flatten
    val lda = new LDAWrap(corpus, 100)
    val articleList = List(14,169) map articles
    articleList foreach { x =>
      val articleParts = GuardianDataSet.Tool.splitArticle(x.article)
      val rel = lda.estimate(x.article, articleParts)
      (articleParts zip rel) foreach (pair => println("article : (" + pair._2 + ") " +  pair._1))
    }
  }

  test("Sentence Viewer"){

    val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    val label = GuardianDataSet.Tool.loadLabelAt("C:\\work\\Data\\guardian data\\labels")

    val articleList = List(20, 21, 22, 23, 24) map articles
    val contrvArticles = articleList.filter(x => label.contains(x) && label.labels(x.id))
    contrvArticles foreach { x =>
      val articleParts = GuardianDataSet.Tool.splitArticle(x.article)
      println("title : " + x.title)
      articleParts foreach println
    }
  }

  test("sentence ranking with any 30 comments")
  {
    println("sentence ranking with any 30 comments")
    val article : Seq[ArticleStructure] = GuardianDataSet.Tool.getAll().toSeq
    val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    def extract( a : ArticleStructure) : List[String] = {
      a.article :: a.comments.slice(0,10)
    }
    val corpus = (articles map extract).flatten
    val lda = new LDAWrap(corpus, 100)
    val articleList = List(14,169, 222) map articles
    articleList foreach { x =>
      val comment = x.comments.slice(10,40).mkString(" ")
      val articleParts = GuardianDataSet.Tool.splitArticle(x.article)
      val rel = lda.estimate(comment, articleParts)
      println("title : " + x.title)
      var idx = 0
      (articleParts zip rel) foreach (pair => println("(%.1f) %s".format(pair._2, pair._1)))
    }
  }


  test("sentence ranking with top 30 contrv comments")
  {
    val article : Seq[ArticleStructure] = GuardianDataSet.Tool.getAll().toSeq
    val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    def extract( a : ArticleStructure) : List[String] = {
      a.article :: a.comments.slice(0,10)
    }
    println("sentence ranking with top 30 contrv comments")
    val featureGen = new FeatureGenerator()
    val ranker = new CICommentRanker(featureGen)

    val corpus = (articles map extract).flatten
    val lda = new LDAWrap(corpus, 100)
    val articleList = List(14,169, 222) map articles
    articleList foreach { x =>
      val topComments : List[String] = ranker.topK(x, 30)
      val source = topComments.mkString(" ")

      val articleParts = GuardianDataSet.Tool.splitArticle(x.article)
      val rel = lda.estimate(source, articleParts)
      println("title : " + x.title)
      var idx = 0
      (articleParts zip rel) foreach (pair => println("(%.1f) %s".format(pair._2, pair._1)))
    }
  }

  test("cossim ranking with top 30 contrv comments")
  {
    val article : Seq[ArticleStructure] = GuardianDataSet.Tool.getAll().toSeq
    val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    def extract( a : ArticleStructure) : List[String] = {
      a.article :: a.comments.slice(0,10)
    }
    println("cossim ranking with top 30 contrv comments")
    val featureGen = new FeatureGenerator()
    val ranker = new CICommentRanker(featureGen)
    val predictor = new EmbeddingCorr()

    val corpus = (articles map extract).flatten
    val articleList = List(14,169, 222) map articles
    articleList foreach { x =>
      val topComments : List[String] = ranker.topK(x, 30)

      val articleParts = GuardianDataSet.Tool.splitArticle(x.article)
      val rel = predictor.predict(topComments, articleParts)
      println("title : " + x.title)
      (articleParts zip rel) foreach (pair => println("(%.1f) %s".format(pair._2, pair._1)))
    }
  }
  test("cossim top tokens")
  {
    val article : Seq[ArticleStructure] = GuardianDataSet.Tool.getAll().toSeq
    val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    val featureGen = new FeatureGenerator()
    val ranker = new CICommentRanker(featureGen)
    val predictor = new EmbeddingCorr()
    val articleList = List(14,169, 222) map articles
    articleList foreach { x =>
      val topComments : List[String] = ranker.topK(x, 30)

      val articleParts = GuardianDataSet.Tool.splitArticle(x.article)
      val topTokens = predictor.topTokens(topComments, articleParts)
      println("title : " + x.title)
      topTokens foreach println
    }
  }
}
