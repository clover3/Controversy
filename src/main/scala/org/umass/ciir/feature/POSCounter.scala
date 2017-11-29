package org.umass.ciir.feature

import edu.stanford.nlp.tagger.maxent.MaxentTagger

class POSCounter(simple:Boolean = true) {
  val tagger = new MaxentTagger("models/english-left3words-distsim.tagger")
  val fullPOSList = List("CC","CD","DT","EX","FW","IN","JJ","JJR","JJS","LS","MD",
    "NN","NNS","NNP","NNPS","PDT","POS","PRP","PRP$",
    "RB","RBR","RBS","RP","SYM","TO","UH","VB",
    "VBD","VBG","VBN","VBP","VBZ","WDT","WP","WP$","WR")
  val simpePOSList = (fullPOSList map (_.take(2))).toSet.toList

  val POSList = if(simple) simpePOSList else fullPOSList

  def count(input:String) : Map[String, Int] = {
    val taggedString = tagger.tagString(input)
    def split_tag(rawTag: String) : (String,String) = {
      val loc = rawTag.lastIndexOf("_")
      val word = rawTag.substring(0,loc)
      val fullTag = rawTag.substring(loc+1)

      if(simple)
        (word,fullTag.take(2))
      else
        (word,fullTag)
    }
    val tokens = taggedString.split(" ") map split_tag
    val posGroup = (tokens groupBy ( _._2))

    posGroup map (x => (x._1, x._2.size))
  }

  def getVector(input: String) : List[Double] = {
    val (pos, counts) = count(input) unzip
    val total = counts.sum
    val portion = counts map (_.toDouble / total)
    val portionMap = (pos zip portion).toMap

    POSList map { pos =>
      if (portionMap.contains(pos)) portionMap(pos)
      else 0
    }
  }
}
