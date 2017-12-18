package org.umass.ciir.dataset


import scala.io.Source._


class ControversyList(path: String) {
  def parseLine(line:String) : String = {
    def dropAfter(s: String, c : String) : String  = {
      val idx = s.indexOf(c)
      if( idx > 0)
        s.substring(0, idx)
      else if( idx < 0 )
        s
      else
        throw new Exception()
    }

    def dropAfterAny(s: String , dList : List[String]) : String = {
      dList match {
        case Nil => s
        case dList => dropAfterAny(dropAfter(s, dList.head), dList.tail)
      }
    }

    val delimiter = List("(", " - ", ",", " – ")
    val s1 = dropAfterAny(line, delimiter)
    val s4 = dropAfter(s1, "and")
    s4.trim
  }
  val list = (fromFile(path).getLines() map parseLine)

}
