import org.umass.ciir.dataset.CommentAgree
import org.umass.ciir.dataset.GuardianDataSet.Tool

object CommentAgreeTest {
  def main(args: Array[String]): Unit = {
    val commentAgree = new CommentAgree("C:\\work\\Data\\guardian data\\comment_agree_big.txt")
    println("Counting existsing comments")
    val printTuple :PartialFunction[(String, Double), Unit] = {case (k,v) => println("%s : %f".format(k,v)) }

    commentAgree.agreePortion foreach printTuple
    commentAgree.disagreePortion foreach printTuple
  }
}
