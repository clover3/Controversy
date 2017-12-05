import org.scalatest.FunSuite
import org.umass.ciir.feature.OverlapFeature

class featureTest extends FunSuite {
  test("commonToken"){
    val str1 = "Hello I am clover how about you?"
    val str2 = "Go to Hell clover "
    assert(OverlapFeature.commonToken(str1, str2)== 1)

    val str3 = "What counts as a token in NLP? The notion of a token must first be defined before computational processing can proceed"
    val str4 = "There is more to the issue than simply identifying strings delimited on both sides by spaces or punctuation"
    assert(OverlapFeature.commonToken(str3, str4)== 0)


    val str5 = "Is he king?"
    val str6 = "He is King."
    assert(OverlapFeature.commonToken(str5, str6)== 0)

  }

}
