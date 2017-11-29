package org.umass.ciir.svm

import scala.collection.JavaConverters._
import libsvm._

package SVMWrap {

  class Instance(sparseVector: scala.collection.immutable.Map[Int, Double]) {
    val array = new Array[svm_node](sparseVector.size)
    val instance: Array[svm_node] = (sparseVector map { pair =>
      val node = new svm_node
      node.index = pair._1
      node.value = pair._2
      node
    }).toArray
  }

  class Model(inst: Array[Instance], label: Array[Double]) {
    val param = new svm_parameter()
    param.svm_type = svm_parameter.C_SVC
    param.kernel_type = svm_parameter.RBF
    param.gamma = 0.5
    param.nu = 0.5
    param.cache_size = 20000
    param.C = 1
    param.eps = 0.001

    val prob = new svm_problem()
    prob.l = inst.size
    prob.y = label
    prob.x = inst map (_.instance)

    val model = svm.svm_train(prob, param)

    def predict(sample: Instance): Double = {
      svm.svm_predict(model, sample.instance)
    }
  }
}