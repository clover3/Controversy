import org.scalatest.FunSuite
import org.umass.ciir.dataset.SentenceLabel

class SentenceLabelTest extends FunSuite {

  test("Simple"){
    val sl = new SentenceLabel("C:\\work\\Data\\guardian data\\Sentence Label")
    sl.articles foreach {
      a => a.data foreach ( p => println(p))
    }
  }

}
