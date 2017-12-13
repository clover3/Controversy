import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import play.api.libs.json._
import com.github.tototoshi.csv.CSVReader
import org.scalatest._
import org.umass.ciir.feature.GuardianDataSet
import org.umass.ciir.feature.GuardianDataSet.ArticleStructure

class GuardianTest extends FunSuite {
  test("expand agree/disagree")
  {
    val folderPath = "C:\\work\\Data\\guardian data\\experiment3-20170120T145743Z\\experiment3\\"
    val reader = CSVReader.open(new File(folderPath + "f978651.csv"))
    val data = reader.all()
    reader.close()
    println(data.transpose.head)

  }

  test("Article Parser")
  {
    def getListOfFiles(dir: String):List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }
    val files :List[File]= getListOfFiles("C:\\work\\Data\\guardian data\\codedpages")
    val articles = files map (x => new GuardianDataSet.ArticleStructure(x.getAbsolutePath()))

    articles foreach { x =>
      println(x.article)
    }
  }

  test("Article Split Test"){
    val article : ArticleStructure = GuardianDataSet.Tool.getOne()
    val paragraphs = GuardianDataSet.Tool.splitArticle(article.article)
    paragraphs foreach println
  }

  test("Guardian to ArsTechnica Format"){
    val articles = GuardianDataSet.Tool.getAll()
    var counter = 0
    def getName() : String = {
      counter = counter +1
      "%d.json".format(counter)
    }
    val articlePath = "guardianArticles\\"
    val commentPath = "guardianComments\\"
    articles foreach { article =>
      val sents = GuardianDataSet.Tool.splitArticle(article.article) map {
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
        val f = new PrintWriter(new File(articlePath + article.id))
        f.write(jObj.toString())
        f.close()

        article.commentExs.slice(0,50) foreach { commentEx =>
          val jObj = JsObject(Seq(
            "title" -> JsString("dummy"),
            "content" -> JsString(commentEx("text")),
            "parent" -> JsString(article.id),
            "cdate" -> JsString(commentEx("timestamp")),
            "name" -> JsString(commentEx("id")),
          ))
          val f = new PrintWriter(new File(commentPath + commentEx("id")))
          f.write(jObj.toString())
          f.close()
       }
      }
    }

  }

  test("Label Test"){
    val label = GuardianDataSet.Tool.loadLabelAt("C:\\work\\Data\\guardian data\\labels").labels
    println("Total of %d labels".format(label.size))
    label.slice(0,10) foreach println
    val id1 = "society_2016_oct_25_speak-polish-saturday-school-language-history"
    //assert(false === label(id1))
    val id2 = "money_2016_oct_27_maternity-leave-sackings-cost-280m-a-year-says-equality-watchdog"
    //assert(true === label(id2))

    val values : Iterable[Boolean]= (label unzip)._2
    val nControversy : Int = values.count((x=>x))
    println("%d of %d is controversial".format(nControversy, label.size))
  }

  test("Corpus label count"){
    val articles : List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    val label = GuardianDataSet.Tool.loadLabelAt("C:\\work\\Data\\guardian data\\labels")
    val validArticles = articles filter (label.contains)
    println("%d of %d has label".format(validArticles.size, articles.size ))


    val numTrue = validArticles count label.get
    println("%d of %d is controversial(%f)".format(numTrue, validArticles.size, numTrue.toDouble / validArticles.size))

  }

  test("Article title viewer"){
    val articles : Seq[ArticleStructure] = GuardianDataSet.Tool.getAll().toSeq
    val w = new BufferedWriter(new FileWriter(new File("article_title.txt")))
    Range(0,articles.length) foreach { i =>
      val s = "%d\t".format(i) + articles(i).title
      w.write(s)
      w.newLine()
    }
  }

}