import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import org.scalatest.FunSuite
import org.umass.ciir.classifier.{ContrClassifier, LingClassifier, RandomClassifier}
import org.umass.ciir.feature.{FeatureGenerator, GuardianDataSet}
import org.umass.ciir.feature.GuardianDataSet.ArticleStructure

import scala.util.Random
import org.umass.ciir.miscLib._
import org.umass.ciir.svm.SVMWrap.ParameterTune

class trainTest extends FunSuite {
  test("ContrClassifier") {

    Random.setSeed(0)
    //println("Pickling...")
    //GuardianDataSet.Tool.pickleArticle()

    val articles: List[ArticleStructure] = time(GuardianDataSet.Tool.getAll(), "Loading Articles")

    val label = time(GuardianDataSet.Tool.loadLabelAt("C:\\work\\Data\\guardian data\\labels"), "Loading Labels")

    val allX = Random.shuffle(articles filter label.contains)
    val trainSize: Int = (allX.size * 0.8).toInt
    val (trainX, testX) = allX.splitAt(trainSize)
    println("Train size =%d Test size = %d".format(trainX.size, testX.size))


    def printAccuracy(classifier: ContrClassifier, X: List[ArticleStructure]) = {
      val (acc, fmeasure, prec, recall) = classifier.accuracy_precison(X)
      println(f"acc : $acc%f fmeasure : $fmeasure%f prec :$prec%f recall : $recall%f")
    }

    val featureGenerator = new FeatureGenerator()
    val lingClassifier = time(new LingClassifier(trainX, label)(featureGenerator), "Training...")
    val randomClassifier = new RandomClassifier(trainX, label)


    print("Train\t")
    printAccuracy(lingClassifier, trainX)
    print("Test\t")
    printAccuracy(lingClassifier, testX)
    print("Random Test\t")
    printAccuracy(randomClassifier, testX)
  }

  test("Parameter Tune")
  {
    Random.setSeed(0)
    val articles: List[ArticleStructure] = GuardianDataSet.Tool.getAll()

    val label = time(GuardianDataSet.Tool.loadLabelAt("C:\\work\\Data\\guardian data\\labels"), "Loading Labels")

    val allX = Random.shuffle(articles filter label.contains)
    val trainSize: Int = (allX.size * 0.8).toInt
    val (trainX, testX) = allX.splitAt(trainSize)
    println("Train size =%d Test size = %d".format(trainX.size, testX.size))

    val feature = new FeatureGenerator()
    val instances = trainX map (feature(_))
    val Y = trainX map label.getD
    assert( (Y count (_ >0.4))   > 10 )
    assert( (Y count (_ < -0.4)) > 10 )

    val tuner = new ParameterTune(instances, Y.toArray)
    println("Best:" +tuner.best)
    println(tuner.report)
  }
}
