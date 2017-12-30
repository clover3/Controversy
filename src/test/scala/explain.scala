import org.umass.ciir.dataset.GuardianDataSet.{ArticleStructure, Tool}
import org.umass.ciir.miscLib.time
import org.umass.ciir.query.Explain

object explain {

  def main(args: Array[String]): Unit = {
    val explain = new Explain("input\\score.txt", "input\\RelevanceCorpusBigram.txt")
    val articlesRaw: List[ArticleStructure] = time(Tool.getAll(), "Loading articles..")
    val articles = articlesRaw.dropRight(50)

    val label = Tool.loadLabelAt("C:\\work\\Data\\guardian data\\labels")
    val contrvArticles = articles filter ( a=> label.labels.contains(a.id) && label.get(a))
    contrvArticles foreach { article =>
      println("Title : "+ article.title)
      explain.asPhrase(article) foreach (l => println(l.mkString(" ")))
    }

  }
}
