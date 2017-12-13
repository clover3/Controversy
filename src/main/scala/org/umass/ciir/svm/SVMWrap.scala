package org.umass.ciir.svm

import scala.collection.JavaConverters._
import libsvm._
import scala.concurrent.ExecutionContext.Implicits.global

package SVMWrap {

  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration.Duration

  class Instance(sparseVector: scala.collection.immutable.Map[Int, Double]) {
    val array = new Array[svm_node](sparseVector.size)
    val instance: Array[svm_node] = (sparseVector map { pair =>
      val node = new svm_node
      node.index = pair._1
      node.value = pair._2
      node
    }).toArray
    def this(v : Iterable[Double]) = this((Range(0,v.size) zip v).toMap)

  }

  class ParameterTune(instRaw: Iterable[Instance], label: Array[Double]) {
    val inst = instRaw.toArray
    val nFold = 5
    def teeSplit[T](data: Iterable[T])(count: Int): List[(Iterable[T], Iterable[T])] = {
      val size = data.size

      def piece(i: Int) = i * size / count

      List.range(0, nFold) map { i =>
        val (prefix, rest) = data.splitAt(piece(i))
        val (test, postfix) = rest.splitAt(piece(i + 1) - piece(i))
        val train = prefix ++ postfix
        (train, test)
      }
    }

    val data = inst zip label


    def getParam(C : Double, gamma : Double) : svm_parameter = {
      val param = new svm_parameter()
      param.svm_type = svm_parameter.ONE_CLASS
      param.kernel_type = svm_parameter.LINEAR
      param.gamma = gamma
      param.C = C
      param.nu = 0.01
      param
    }

    def getF1(result : Iterable[(Boolean, Boolean)]) : Double = {
      assert(result.size > 1)
      val tp = result count ( x => x._1 && x._2)
      val fp = result count {x:(Boolean,Boolean) => x._1 && !x._2}
      val tn = result count ( x => !x._1 && !x._2)
      val fn = result count ( x => !x._1 && x._2)
      println(tp,fp,tn,fn)
      val precision = tp.toDouble / (tp+fp)
      val recall = tp.toDouble / (tp+fn)
      val fmeasure = 2*(precision*recall) / (precision+recall)
      fmeasure
    }

    def trainAndTest(param : svm_parameter, train: Iterable[(Instance, Double)], test: Iterable[(Instance, Double)]): Double =
    {
      assert(train.size > 1)
      assert(test.size > 1)
      val prob = new svm_problem()
      prob.l = train.size
      val (x,y) = train.unzip
      prob.x = (x map (_.instance)).toArray
      prob.y = y.toArray
      val model = svm.svm_train(prob, param)

      val res : Iterable[(Boolean, Boolean)] = test map { pair =>
        val x = pair._1
        val trueY = pair._2
        val predY = svm.svm_predict(model, x.instance)
        (trueY > 0 ,predY > 0)
      }
      getF1(res)
    }

    def getPerformance(param : svm_parameter) : Double = {
      val v = for{
        (train,test) <- teeSplit(data)(nFold)
      } yield trainAndTest(param, train,test)
      v.sum / v.size
    }


    val gammaCandidate = List(0.0001, .001, 0.01, 0.1 )
    val CCandidate = List(0.00001, 0.0001, 0.01)
    val nuCandidate =List(0, 0.1, 0.2, 0.3, 0.5, 0.6)
    val params = for {
      gamma <- gammaCandidate
      c <- CCandidate
      //nu <- nuCandidate
    } yield getParam(c, gamma)

    val paramResult = params map { p => Future{ (p.C, p.gamma, getPerformance(p) )}}
    val report = Await.result(Future.sequence(paramResult),Duration.Inf)
    val best = report.maxBy(_._3)
  }

  class Model(instRaw: Iterable[Instance], label: Array[Double]) {
    val inst = instRaw.toArray
    val param = new svm_parameter()
    param.svm_type = svm_parameter.ONE_CLASS
    param.kernel_type = svm_parameter.LINEAR
    param.gamma = 0.01
    param.C = 0.001
    param.nu = 0.3

    val prob = new svm_problem()
    prob.l = inst.size
    prob.y = label
    prob.x = inst map (_.instance)
    svm.svm_check_parameter(prob, param)
    val model = svm.svm_train(prob, param)
    //printData()

    def predict(sample: Instance): Double = {
      svm.svm_predict(model, sample.instance)
    }

    def save(path: String) = {
      svm.svm_save_model(path, model)
    }

    def printData(): Unit = {

      val r : Array[Array[(Int, Double)]] = (model.sv_coef.flatten zip model.SV) map { x =>
        val weight : Double = x._1
        val sv : Array[svm_node] = x._2
        sv map (n => (n.index, n.value*weight))
      }
      def sumPair(l : Iterable[(Int, Double)]) = {
        val (a,b) = l.unzip
        b.sum
      }
      val featureWeight : Iterable[(Int, Double)] = (r.flatten.groupBy { x => x._1})map( y => (y._1, sumPair(y._2)))
      featureWeight foreach (x => print("%d->%f, ".format(x._1,x._2) ))
    }

  }
}