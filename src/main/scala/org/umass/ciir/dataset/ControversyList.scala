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
    val s1 = dropAfter(line, "-")
    val s2 = dropAfter(s1, ",")
    val s3 = dropAfter(s2, "(")
    val s4 = dropAfter(s3, "and")
    s4.trim
  }
  val list = fromFile(path).getLines() map parseLine

}
