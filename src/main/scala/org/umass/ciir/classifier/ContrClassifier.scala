package org.umass.ciir.classifier

import org.umass.ciir.feature.GuardianDataSet.{ArticleStructure, ControversyLabel}


abstract class ContrClassifier (X : List[ArticleStructure], label: ControversyLabel) {

  val validX = X.filter(label.contains)
  val Y : List[Double] = validX map label.getD

  def test(x: ArticleStructure) : Boolean

  def accuracy_precison(xTest : List[ArticleStructure]): (Double, Double, Double, Double) =
  {
    val testY : List[Boolean] = xTest map test
    val realY : List[Boolean] = xTest map (x=>label.labels(x.id))
    val correctCount = (testY zip realY) count ( x=> x._1 == x._2)
    val accuracy = correctCount.toDouble / xTest.size
    val tp = (testY zip realY) count ( x => x._1 && x._2)
    val fp = (testY zip realY) count {x:(Boolean,Boolean) => x._1 && !x._2}
    val tn = (testY zip realY) count ( x => !x._1 && !x._2)
    val fn = (testY zip realY) count ( x => !x._1 && x._2)
    println(tp,fp,tn,fn)
    val precision = tp.toDouble / (tp+fp)
    val recall = tp.toDouble / (tp+fn)
    val fmeasure = 2*(precision*recall) / (precision+recall)
    (accuracy, fmeasure, precision, recall)
  }
}

