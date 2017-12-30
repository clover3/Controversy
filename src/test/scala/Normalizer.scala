import java.text.Normalizer

import org.scalatest.FunSuite

class Normalizer extends FunSuite{

  test("Normalize"){
    val str = "Opinium surveyed 2,001 adults between 1 and 4 November, a week in which polls narrowed amid the fallout from the FBI’s decision to review a new batch of Clinton’s staff’s emails."
    val normalized_string = str.replaceAll("[^\\x00-\\x7F]", "")
    println(str)
    println(normalized_string)
  }
}
