import org.scalatest._
import org.umass.ciir.feature._

class ExpandSuite extends FunSuite {
  test("expand agree/disagree")
  {
    val seedAgreement = List(
      "agreement",
      "accord",
      "arrangement",
      "compliance",
      "compromise",
      "concession",
      "mediation",
      "reconciliation",
      "understanding")

    val seedDisagreement = List(
      "disagreement",
      "animosity",
      "antagonism",
      "argument",
      "bickering",
      "clash",
      "conflict",
      "controversy",
      "debate",
      "difference",
      "discord",
      "dissent",
      "disunity",
      "division",
      "feud",
      "fight",
      "friction",
      "hostility",
      "misunderstanding",
      "opposition",
      "spat",
      "tension"
    )

    val seedDisagree = List(
      "disagree",
      "bicker",
    "clash",
    "contend",
    "differ",
    "dissent",
    "divide",
    "feud",
    "haggle",
    "object",
    "oppose",
    "quarrel",
    "quibble",
    "spar",
    "wrangle"
    )
    val seedAgree = List(
      "agree",
      "acknowledge",
  "admit",
  "allow",
  "comply",
  "concede",
  "concur",
  "grant",
  "recognize",
  "set",
  "settle",
  "sign"
    )

    val expanded = new LexiconExpander(seedAgree, seedDisagree, 200)
    println(expanded.aList)
    println(expanded.bList)
  }
}
