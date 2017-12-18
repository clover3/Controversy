package dataset

import org.scalatest.FunSuite
import org.umass.ciir.dataset.ControversyList
import org.umass.ciir.feature.Tokenizer

class ControversyListTest extends FunSuite{
  test("simple load"){
    val cl = new ControversyList("resource\\controversyList.txt")
    cl.list foreach println
  }

  test("Normalizer Test"){
    val cl = new ControversyList("resource\\controversyList.txt")
    val tokenizer = new Tokenizer()
    cl.list foreach {  s =>
      val tokens = tokenizer.TokenizerNormalizeStemmer(s)
      println(tokens.mkString("/"))
    }
  }
}
