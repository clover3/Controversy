package classifier


import java.io._

import org.scalatest.FunSuite
import org.umass.ciir.dataset.GuardianDataSet._

import scala.io.Source

class agreeClassifierSuite extends FunSuite{
  test("output comments"){
    val articles : Seq[ArticleStructure] = Tool.getAll()

    val allComments : Iterable[String] = (articles map (a => a.comments)).flatten
    val allCommentsNoNewline = allComments map (_.replace("\n", " ").replace("\r", " "))
    val w = new BufferedWriter(new FileWriter(new File("allComment.txt")))
    allCommentsNoNewline foreach { comment =>
      w.write(comment)
      w.newLine()
    }
    w.close()
  }
  test("load agreement result")
  {
    val lines = Source.fromFile("resource\\commentAgree.txt").getLines.toList
    def parseLine(line: String) : (String, Int) = {
      (line.substring(2), line.substring(0,1).toInt)
    }
    val agreeClassifier = (lines map parseLine).toMap
    println("length : "+ agreeClassifier.size.toString)
    agreeClassifier.keys.slice(0,10) foreach println

    println(agreeClassifier.values.slice(0,1000))
  }


}
