package org.umass.ciir.classifier

import org.umass.ciir.dataset.GuardianDataSet._

import scala.util.Random

class RandomClassifier(X : List[ArticleStructure], label: ControversyLabel) extends ContrClassifier(X,label)
{
  val posPortion = (Y count ( _ > 0)).toDouble / Y.size
  def test(x: ArticleStructure) : Boolean = {
    Random.nextDouble() < posPortion
  }
}