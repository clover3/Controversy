import java.io.{File, PrintWriter}

import org.umass.ciir.dataset.GuardianDataSet.{ArticleStructure, Tool}
import org.umass.ciir.feature.SimpleTokenzier
import org.umass.ciir.miscLib.time
import org.umass.ciir.query.RM3

import scala.io.Source.fromFile

object queryExpandTest {
  def main(args: Array[String]): Unit = {
    val tokenizerS = new SimpleTokenzier()

    val articlesRaw: List[ArticleStructure] = time(Tool.getAll(), "Loading articles..")
    val articles = articlesRaw.dropRight(50)

    val bigramQuery = (fromFile("output\\guardianBigram.txt").getLines() map (_.split(" ").toList)).toList

    def subQuery2String(v: Map[String, Double]) = (v.toList.sortBy(-
      _._2) map { case(word,weight) => "%s %f".format(word,weight)}).mkString("\t")
    val rm3 = new RM3(bigramQuery, articles, tokenizerS)

    val fc = new PrintWriter(new File("output\\expandedBigram.txt"))

    rm3.extQuery foreach{
      case (k,v) => fc.write(k.mkString(" ") + "\t" + subQuery2String(v) + "\n")
    }
  }
}
