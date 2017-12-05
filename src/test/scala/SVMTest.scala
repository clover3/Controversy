import org.scalatest.FunSuite
import libsvm._
import scala.annotation.meta.param
import org.umass.ciir.svm.SVMWrap._


class SVMTest extends  FunSuite{

  test("Simple svm"){
    val param = new svm_parameter()

    param.svm_type = svm_parameter.C_SVC
    param.kernel_type = svm_parameter.RBF
    param.gamma = 0.5
    param.nu = 0.5
    param.cache_size = 20000
    param.C = 1
    param.eps = 0.001

    val prob = new svm_problem()
    prob.l = 100
    prob.y = new Array[Double](prob.l)
    prob.x = new Array[Array[svm_node]](prob.l)
    Range(0,100) foreach {i =>
      if(i % 2 == 0 ) {
        prob.y(i) = 1
        val array = new Array[svm_node](2)
        array(0) = new svm_node
        array(0).index = 1
        array(0).value = i
        array(1) = new svm_node
        array(1).index = 2
        array(1).value = 1
        prob.x(i) = array
      }
      else{
        prob.y(i) = 2
        val array = new Array[svm_node](2)
        array(0) = new svm_node
        array(0).index = 1
        array(0).value = i
        array(1) = new svm_node
        array(1).index = 2
        array(1).value = 0
        prob.x(i) = array
      }
    }
    println(svm.svm_check_parameter(prob, param))
    val model = svm.svm_train(prob, param)

    val node1 = new Array[svm_node](2)
    node1(0) = new svm_node
    node1(0).index = 1
    node1(0).value = 18
    node1(1) = new svm_node
    node1(1).index = 2
    node1(1).value = 0
    
    val node2 = new Array[svm_node](2)
    node2(0) = new svm_node
    node2(0).index = 1
    node2(0).value = 18
    node2(1) = new svm_node
    node2(1).index = 2
    node2(1).value = 1
    println(svm.svm_predict(model, node1));
    println(svm.svm_predict(model, node2));
  }

  test("wrapper svm"){
    val instance : IndexedSeq[Instance] = Range(0,100) map {i =>
      val m1 = List((1,i.toDouble), (2,((i+1)%2).toDouble)).toMap
      val m : scala.collection.immutable.Map[Int,Double] = m1
      new Instance(m)
    }
    val labels = Range(0,100) map {i => (i+2)%2 +1.0 }
    val model = new Model(instance, labels.toArray)

    val test1 = new Instance(List((1,104.0), (2,0.0)).toMap)
    val test2 = new Instance(List((1,18.0), (2,1.0)).toMap)

    println(model.predict(test1))
    println(model.predict(test2))
  }
}
