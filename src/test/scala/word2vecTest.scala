import java.io.{BufferedOutputStream, File, FileOutputStream, PrintWriter}

import scala.collection.JavaConverters._
import org.insight.wordspace.W2vSpace
import org.scalatest.FunSuite

class word2vecTest extends FunSuite{

  test("Word2vec words"){
    val w2v : W2vSpace = W2vSpace.load("C:\\work\\Data\\GoogleNews-vectors-negative300.bin\\GoogleNews-vectors-negative300.bin")
    val writer = new PrintWriter(new File("Voca.txt"))
    val keys = w2v.store.keySet()
    keys.asScala foreach {x => writer.write(x + "\n")}
  }
}
