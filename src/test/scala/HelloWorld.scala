

import org.scalatest._

import scala.io.Source
class HelloWorld extends FunSuite {
  test("helloWorld") {
    print("HelloWorld")

  }

  test("read file"){
    val lines = Source.fromFile("resource\\bad-words.txt").getLines.toList
    print(lines(0))
    print(lines(1))
  }
}
