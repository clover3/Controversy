package org.umass.ciir.feature

import scala.io.Source._
import scala.xml.SAXParseException
import java.io.File

import com.github.tototoshi.csv._
import play.api.libs.json._

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import net.ruippeixotog.scalascraper.model._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import scala.concurrent.ExecutionContext.Implicits.global

package GuardianDataSet {

  import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

  import scala.collection.mutable
  import scala.concurrent.duration.Duration
  import scala.concurrent.{Await, Future}

  @SerialVersionUID(100L)
  class ArticleStructure(path: String) extends Serializable {
    val rawJson: String = fromFile(path, "utf-8").mkString("").replace("\n","")
    val jsonObj = Json.parse(rawJson)

    val article = (jsonObj \ "response" \ "content" \ "blocks" \ "body" \ 0 \ "bodyHtml").asOpt[String].get
    val commentNode = (jsonObj \ "response" \ "content" \ "blocks"\ "comments").asOpt[JsArray].get
    val title = (jsonObj \ "response" \ "content" \ "webTitle").asOpt[String].get
    def parseComment(node : JsValue) : String = (node \ "text").asOpt[String].get
    def parseCommentEx(node : JsValue) : Map[String, String] = Map(
      "text" -> (node \ "text").asOpt[String].get,
      "id" -> (node \ "id").asOpt[String].get,
      "timestamp" -> (node \ "timestamp").asOpt[String].get,
      "reply-to" -> (node \ "reply-to").asOpt[String].get
    )
    val comments = (commentNode.value map parseComment).toList
    val commentExs = (commentNode.value map parseCommentEx).toList
    val id = (jsonObj \ "response" \ "content" \ "id").asOpt[String].get.replace("/","_")
    val date = (jsonObj \ "response" \ "content" \ "webPublicationDate").asOpt[String].get

    override def toString: String = {
      article.toString + title.toString + comments.toString() + commentExs.toString() + id.toString + date.toString
    }
  }

  class ControversyLabel(paths: List[String]){
    def getData(path: String) = {
      val reader = CSVReader.open(new File(path))
      val data = reader.all()
      reader.close()
      data
    }
    val datas = paths map getData

    val widths = datas map (_.head.size)

    assert(widths.forall{_ == widths.head})
    val column = datas.head.head
    val allDataList : List[List[String]] = (datas map (_.tail)).flatten
    val allData : List[IndexedSeq[String]] = allDataList map (_.toIndexedSeq)
    val urlIdx = column.indexOf("url")

    def idPair(entry : IndexedSeq[String]) : (String, IndexedSeq[String]) = {
      val url = entry(urlIdx)
      assert( url.substring(23,28) == ".com/")
      val id = url.substring(28).replace("/","_")
      (id, entry)
    }
    val indexedData = (allData map idPair)
    val group = indexedData.groupBy(_._1)
    val labelIdx = column.indexOf("controversial")
    val labels = group map { x =>
      val id = x._1
      val data = (x._2 unzip)._2
      val labels : List[String] = data map (x => x(labelIdx))
      val score = (labels map {case "yes" => 1
                                case "no" => 0}).sum
      if (score*2 > labels.size) (id,true)
      else (id,false)
    }

    def contains(x:ArticleStructure) : Boolean = labels.contains(x.id)
    def getD(x:ArticleStructure) : Double = {
      if(labels(x.id)) 1
      else -1
    }
    def get(x:ArticleStructure) : Boolean = {
      labels(x.id)
    }
  }

  object Tool {
    val picklePath = "f:\\article.pickle"
    def getAll() : List[ArticleStructure] = {
      val f= new File(picklePath)
      if (false)
      {
        println("Loading from cache")
        val ois = new ObjectInputStream(new FileInputStream(picklePath))
        val obj = ois.readObject.asInstanceOf[List[ArticleStructure]]
        obj
      }
      else
        getAllFromCodedPage()
    }
    def pickleArticle() : Unit = {
      val obj = getAllFromCodedPage()
      val oos = new ObjectOutputStream(new FileOutputStream(picklePath))
      oos.writeObject(obj)
      oos.close
    }

    def getAllFromCodedPage(): List[ArticleStructure]  = {
      def getListOfFiles(dir: String):List[File] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
          d.listFiles.filter(_.isFile).toList
        } else {
          List[File]()
        }
      }
      val files :List[File]= getListOfFiles("C:\\work\\Data\\guardian data\\codedpages")
      val fList = files map (x => Future{new GuardianDataSet.ArticleStructure(x.getAbsolutePath())} )
      fList map {f => Await.result(f, Duration.Inf)}
    }
    def getOne() : ArticleStructure = {
      def getListOfFiles(dir: String):List[File] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
          d.listFiles.filter(_.isFile).toList
        } else {
          List[File]()
        }
      }

      val files :List[File]= getListOfFiles("C:\\work\\Data\\guardian data\\codedpages")
      new GuardianDataSet.ArticleStructure(files.head.getAbsolutePath)
    }
    def sentenceSplit(text: String ) : List[String] = {

      val re = "[^.!?\\s][^.!?]*(?:[.!?](?!['\"]?\\s|$)[^.!?]*)*[.!?]?['\"]?(?=\\s|$)".r

      val sents = re.findAllMatchIn(text) map (_.toString())
      sents.toList
    }
    def splitArticle(articleText : String) : List[String] = {
      try {
        val browser = JsoupBrowser()
        val doc = browser.parseString("<root>" + articleText + "</root>")
        val paras = doc >> elementList("p") map(_ >> allText("p"))
        (paras.toList map sentenceSplit) flatten
      }catch{
        case e : SAXParseException => println(articleText)
          throw e
      }
    }
    def loadLabelAt(dirPath : String) = {
      def getListOfFiles(dir: String):List[String] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
          d.listFiles.filter(_.isFile).toList map (_.getAbsolutePath)
        } else {
          List[String]()
        }
      }
      new ControversyLabel(getListOfFiles(dirPath))
    }
  }

}

