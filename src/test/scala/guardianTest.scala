import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import scala.io.Source._
import play.api.libs.json._
import com.github.tototoshi.csv.CSVReader
import org.scalatest._
import org.umass.ciir.dataset
import org.umass.ciir.dataset.GuardianDataSet.{ArticleStructure, Tool}

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
    val articles = files map (x => new ArticleStructure(x.getAbsolutePath()))

    articles foreach { x =>
      println(x.article)
    }
  }

  test("article comment parse test"){
    Tool.getAll() foreach {
      article =>
        println(article.title)
        println(article.commentExs.size)
    }
  }

  test("Article Split Test"){
    val article : ArticleStructure = Tool.getOne()
    val paragraphs = Tool.splitArticle(article.article)
    paragraphs foreach println
  }

  test("Guardian to ArsTechnica Format"){
    val articles = Tool.getAll()
    var counter = 0
    def getName() : String = {
      counter = counter +1
      "%d.json".format(counter)
    }
    val articlePath = "guardianArticles\\"
    val commentPath = "guardianComments\\"
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
    val label = Tool.loadLabelAt("C:\\work\\Data\\guardian data\\labels").labels
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
    val articles : List[ArticleStructure] = Tool.getAll()
    val label = Tool.loadLabelAt("C:\\work\\Data\\guardian data\\labels")
    val validArticles = articles filter (label.contains)
    println("%d of %d has label".format(validArticles.size, articles.size ))


    val numTrue = validArticles count label.get
    println("%d of %d is controversial(%f)".format(numTrue, validArticles.size, numTrue.toDouble / validArticles.size))

  }

  test("Label agreemnet kappa"){
    val label = Tool.loadLabelAt("C:\\work\\Data\\guardian data\\labels")


    def countAgree(idx1 : Int, idx2 : Int) : (Double, Double) = {
      val f1 = label.rawLabels count( x => x._2(idx1) == "yes")
      val f2 = label.rawLabels count( x => x._2(idx1) == "no")
      val g1 = label.rawLabels count( x => x._2(idx2) == "yes")
      val g2 = label.rawLabels count( x => x._2(idx2) == "no")

      val a =  label.rawLabels count( x => x._2(idx1) == x._2(idx2))
      val N = label.rawLabels.size

      val p_e :Double = (f1 * g1 + f2 * g2).toDouble / (N*N)
      val p0 : Double = a.toDouble / N
      val agree = a.toDouble / N
      val kappa = (p0-p_e) / (1- p_e)
      (agree, kappa)
    }

    val list = List((0,1), (1,2), (2,3), (3,4), (4,5), (5,0))

    val res = (list map (x => countAgree(x._1, x._2)))
    res foreach{ x=>
      println("%4f %4f".format(x._1, x._2))
    }
  }

  test("Test Shiri agree"){
    val path = "C:\\work\\Data\\Controversy-DataPackage-Updated2015\\Controversy-DataPackage-Updated2015\\judgments\\judgments-raw-and-avg\\judgments-pages-lightTask-2013-05-15.txt"

    def simple(raw: String) : String = raw match {
      case "1" => "1"
      case "2" => "2"
      case "3" => "3"
      case "4" => "4"
    }
    val data = (fromFile(path).getLines() map { s =>
      val arr = s.split("\\s")
      (arr(0), simple(arr(1)))
    }).toList
    val groups = data.groupBy{x => x._1}
    println("Total data : " + data.size)


    val multiLabel = groups.filter(x => x._2.size > 1)
    println("Pages with multi label : ", multiLabel.size)
    val label = (multiLabel map ( x => (x._1, (x._2 map (_._2)).toSeq))).toList
    println(label.head)

    def countAgree(idx1 : Int, idx2 : Int) : (Double, Double) = {
      val f1 = label count( x => x._2(idx1) == "1")
      val f2 = label count( x => x._2(idx1) == "2")
      val f3 = label count( x => x._2(idx1) == "3")
      val f4 = label count( x => x._2(idx1) == "4")
      println(s"$f1 $f2 $f3 $f4")
      val g1 = label count( x => x._2(idx2) == "1")
      val g2 = label count( x => x._2(idx2) == "2")
      val g3 = label count( x => x._2(idx2) == "3")
      val g4 = label count( x => x._2(idx2) == "4")
      println(s"$g1 $g2 $g3 $g4")

      val a =  label count( x => x._2(idx1) == x._2(idx2))
      val N = label.size

      val p_e :Double = (f1 * g1 + f2 * g2).toDouble / (N*N)
      val p0 : Double = a.toDouble / N
      val agree = a.toDouble / N
      val kappa = (p0-p_e) / (1- p_e)
      (agree, kappa)
    }
    println(countAgree(0,1))




  }

  test("Article title viewer"){
    val articles : Seq[ArticleStructure] = Tool.getAll().toSeq
    val w = new BufferedWriter(new FileWriter(new File("article_title.txt")))
    Range(0,articles.length) foreach { i =>
      val s = "%d\t".format(i) + articles(i).title
      w.write(s)
      w.newLine()
    }
  }

  test("Article comment counter"){
    val articles : Seq[ArticleStructure] = Tool.getAll().toSeq
    articles foreach { a =>
      println("%s comment %d - %d".format(a.title, a.comments.size, a.commentExs.size))
    }
  }

  test("Article Split to label"){
    val w = new BufferedWriter(new FileWriter(new File("apple.txt")))
    val articles : List[ArticleStructure] = Tool.getAll()
    articles foreach {
      article =>
        if (article.title.contains("Apple has a blip")) {
          val paragraphs = Tool.splitArticle(article.article)
          var idx = 0
          paragraphs foreach { s =>
            w.write("%d\t%d\t%s\n".format(idx, 0, s))
            idx = idx + 1
          }
        }
    }
    w.close()
  }
}