package org.umass.ciir.classifier

import org.umass.ciir.feature.GuardianDataSet.{ArticleStructure, ControversyLabel}
import org.umass.ciir.feature._
import org.umass.ciir.svm.SVMWrap.{Instance, Model}

class LingClassifier (X : List[ArticleStructure], label: ControversyLabel)(implicit featureGen: FeatureGenerator) extends ContrClassifier(X, label)
{
  val instances = validX map (featureGen(_))
  val model = new Model(instances, Y.toArray)

  def test(x: ArticleStructure) : Boolean = {
    val d = model.predict(featureGen(x))
    if(d > 0) true
    else false
  }
}
