package org.umass.ciir.classifier

import org.umass.ciir.feature.{CommentRegression, FeatureGenerator}
import org.umass.ciir.dataset.GuardianDataSet._


class LinearClassifier (X : List[ArticleStructure], label: ControversyLabel) (implicit featureGen: FeatureGenerator)
  extends ContrClassifier(X, label)
{
  val commentRegression = new CommentRegression(featureGen)

  def test(x: ArticleStructure) : Boolean = {


    val d = commentRegression(x)
    if(d > 0) true
    else false
  }

}
