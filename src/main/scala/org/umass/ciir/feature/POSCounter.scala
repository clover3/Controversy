package org.umass.ciir.feature

import java.io._

import edu.stanford.nlp.tagger.maxent.MaxentTagger

import scala.collection.mutable

object POSOption extends Enumeration {
  val Simple = 1
  val Full = 2
  val Restricted = 3
}

class POSCounter( mode:Int = POSOption.Simple) {
  lazy val tagger = new MaxentTagger("models/english-left3words-distsim.tagger")
  val fullPOSList = List("CC","CD","DT","EX","FW","IN","JJ","JJR","JJS","LS","MD",
    "NN","NNS","NNP","NNPS","PDT","POS","PRP","PRP$",
    "RB","RBR","RBS","RP","SYM","TO","UH","VB",
    "VBD","VBG","VBN","VBP","VBZ","WDT","WP","WP$","WR")
  val simpePOSList = (fullPOSList map (_.take(2))).toSet.toList
  val restrictedPOSList = List("NN","PR", "RB", "JJ", "MD","UH")

  val POSList = mode match{
    case POSOption.Simple => simpePOSList
    case POSOption.Full => fullPOSList
    case POSOption.Restricted => restrictedPOSList
  }


  def loadCache() : mutable.Map[String, List[Double]] = {
    try {
      val ois = new ObjectInputStream(new FileInputStream("POSCache"))
      val cache = ois.readObject.asInstanceOf[mutable.Map[String, List[Double]]]
      val cacheSize= cache.size
      ois.close
      cache
    } catch{
      case e: FileNotFoundException => mutable.Map[String,List[Double]]()
    }
  }
  val cache = loadCache()
  def saveCache() = {
    val oos = new ObjectOutputStream(new FileOutputStream("POSCache"))
    val cacheSize = cache.size
    println(f"saved $cacheSize%d caches")
    oos.writeObject(cache)
    oos.close
  }

  def count(input:String) : Map[String, Int] = {
    val taggedString = tagger.tagString(input)
    def split_tag(rawTag: String) : (String,String) = {
      val loc = rawTag.lastIndexOf("_")
      val word = rawTag.substring(0,loc)
      val fullTag = rawTag.substring(loc+1)

      if(mode == POSOption.Full)
        (word,fullTag)
      else
        (word,fullTag.take(2))
    }
    val tokens = taggedString.split(" ") map split_tag
    val posGroup = (tokens groupBy ( _._2))

    val countMap = posGroup map (x => (x._1, x._2.size))
    mode match {
      case POSOption.Restricted => countMap.filterKeys(restrictedPOSList.toSet.contains(_))
      case POSOption.Simple => countMap
      case POSOption.Full => countMap
    }
  }

  def getVector(input: String) : List[Double] = {
    if (cache.contains(input)) cache(input)
    else {
      val r = getVectorDirect(input)
      cache.put(input,r)
      r
    }
  }

  def getVectorDirect(input: String) : List[Double] = {
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
