package feature

import org.umass.ciir.feature.CommentAnalyzer

object CommentAnalyzerRunner {
  def main(args: Array[String]): Unit = {
    val ca = new CommentAnalyzer("C:\\work\\Data\\guardian data\\crawl_comment")
  }
}
