import org.scalatest.FunSuite
import org.umass.ciir.feature.TokenizerForTopic
import java.text.Normalizer

class TokenizerTest extends FunSuite{
  test("Quote Test"){
    val tokenizer = new TokenizerForTopic()
    val s = "Katie Price was described as having “confessed” when she said she probably would have had an abortion if she’d known her son Harvey was going to be severely disabled, as if the thought, let alone the act, was a heinous crime."
    val subjectString = Normalizer.normalize(s, Normalizer.Form.NFKD)
    val s2 = s.replaceAll( "[^\\x00-\\x7F]", "" )
    println(s)
    println(subjectString)
    tokenizer.TokenizerNormalizeStemmer(s2) foreach println
  }

}
