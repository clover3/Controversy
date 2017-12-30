package org.umass.ciir.feature

import org.umass.ciir.dataset.GuardianDataSet._

class CommentRegression(featureGen: FeatureGenerator)
{

  // 0 Agree ( neural )
  // 1 Disagree ( neural )
  // 2 offensive words %
  // 3 quotation %
  // 4 agree words
  // 5 disagree words
  // 6 POS - NN
  // 7 POS - PR
  // 8 POS - RB
  // 9 POS - JJ

  val weights : Map[Int, Double] = Map(
    0->33.631177,
    5->0.823592,
    10->6.025757,
    1->71.897880,
    6->85.091818,
    9->28.817287,
    13->51.130588,
    2->1.082153,
    12->7.916237,
    7->28.182866,
    3->6.058194,
    11->0.000000,
    8->24.682272,
    4->8.500743)
  val rho = 114.04540195465088
  def getWeight(i: Int) : Double = {
    if( weights.contains(i) )
      weights(i)
    else
      0
  }

  def apply(x: ArticleStructure) : Double= {
    val f = featureGen.getVector(x).toArray
    apply(f)
  }

  def apply(f: Array[Double]):Double = {
    assert(f.size > 13)
      val weighted = for{
      i <- 0 until f.size
    } yield f(i) * getWeight(i)
    weighted.sum - rho
  }
}
