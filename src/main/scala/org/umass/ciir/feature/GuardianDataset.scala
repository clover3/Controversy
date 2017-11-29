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

package GuardianDataSet {


  class ArticleStructure(path: String) {
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
    )
    val comments = (commentNode.value map parseComment).toList
    val commentExs = (commentNode.value map parseCommentEx).toList
    val id = (jsonObj \ "response" \ "content" \ "id").asOpt[String].get.replace("/","_")
    val date = (jsonObj \ "response" \ "content" \ "webPublicationDate").asOpt[String].get
  }

  object Tool {
    def getAll() : List[ArticleStructure] = {
      def getListOfFiles(dir: String):List[File] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
          d.listFiles.filter(_.isFile).toList
        } else {
          List[File]()
        }
      }

      val files :List[File]= getListOfFiles("C:\\work\\Data\\guardian data\\codedpages")
      files map (x => new GuardianDataSet.ArticleStructure(x.getAbsolutePath()))
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
  }

}

