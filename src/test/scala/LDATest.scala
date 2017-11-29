import java.io.File

import org.scalatest.FunSuite
import org.umass.ciir.feature.{GuardianDataSet, LDAWrap}
import cc.mallet.types._
import cc.mallet.pipe._
import cc.mallet.pipe.iterator._
import cc.mallet.topics._

import scala.collection.JavaConverters._
import cc.mallet.topics.ParallelTopicModel
import cc.mallet.types.Alphabet
import java.util
import java.util.regex.Pattern

import org.umass.ciir.feature.GuardianDataSet
import org.umass.ciir.feature.GuardianDataSet.ArticleStructure

class LDATest extends FunSuite {
  test("LDA Test"){

    val path = "C:\\work\\Data\\guardian data\\codedpages\\4b9ad79e-931a-4461-b673-160a37421984.json"
    val article = new GuardianDataSet.ArticleStructure(path)
    val corpus = (article.article :: article.comments).asJava

    val pipeList = new util.ArrayList[Pipe]

    // Pipes: lowercase, tokenize, remove stopwords, map to features
    pipeList.add(new CharSequenceLowercase)
    pipeList.add(new CharSequence2TokenSequence(Pattern.compile("\\p{L}[\\p{L}\\p{P}]+\\p{L}")))
    pipeList.add(new TokenSequenceRemoveStopwords(false))
    pipeList.add(new TokenSequence2FeatureSequence)

    val instances = new InstanceList(new SerialPipes(pipeList))

    val itr = new ArrayIterator(corpus)
    instances.addThruPipe(itr)
    val numTopics = 100
    val model = new ParallelTopicModel(numTopics, 1.0, 0.01)

    model.addInstances(instances)

    // Use two parallel samplers, which each look at one half the corpus and combine
    //  statistics after every iteration.

    // Run the model for 50 iterations and stop (this is for testing only,
    //  for real applications, use 1000 to 2000 iterations)
    model.setNumIterations(50)
    model.estimate

    // Show the words and topics in the first instance

    // The data alphabet maps word IDs to strings
    val dataAlphabet = instances.getDataAlphabet
    val tokens = model.getData.get(0).instance.getData.asInstanceOf[FeatureSequence]
    val topics = model.getData.get(0).topicSequence
    val topicDistribution = model.getTopicProbabilities(0)
    topicDistribution foreach print
  }

  test("LDA Full Test")
  {
    val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    def extract( a : ArticleStructure) : List[String] = {
      a.article :: a.comments.slice(0,10)
    }
    val corpus = (articles map extract).flatten
    val lda = new LDAWrap(corpus)
    println("Num of topic :", lda.numTopics)
    println("Num words : ", lda.words.size())
    println("p size : ", lda.wordsP.size())
    println("p[0] size : ", lda.wordsP.get(0).size())
    def printIDSorter(x :IDSorter) : Unit = {
      println(lda.words.lookupObject(x.getID()), x.getWeight() )
    }
    val idSet : util.TreeSet[IDSorter] = lda.wordsP.get(2)
    idSet.asScala foreach printIDSorter

  }

  test("Mallet Prac")
  {

    val pipeList = new util.ArrayList[Pipe]

    // Pipes: lowercase, tokenize, remove stopwords, map to features
    pipeList.add(new CharSequenceLowercase)
    pipeList.add(new CharSequence2TokenSequence(Pattern.compile("\\p{L}[\\p{L}\\p{P}]+\\p{L}")))
    pipeList.add(new TokenSequenceRemoveStopwords(false))
    pipeList.add(new TokenSequence2FeatureSequence)

    val instances = new InstanceList(new SerialPipes(pipeList))
    val items : java.util.List[String] = (List("Perhaps the 'resources' directories weren't copied into the 'class' directory Hello what was wrong here peoples?")).asJava
    val itr = new ArrayIterator(items)
    instances.addThruPipe(itr)
    val first = instances.get(0)
    println(first)
    val tokens : FeatureSequence= first.getData().asInstanceOf[FeatureSequence]

    val item = for(
      i <- 0 until tokens.getLength()
    ) yield tokens.get(i)
    println(item)
  }

  test("Estimate Tester"){
    val articles :List[ArticleStructure] = GuardianDataSet.Tool.getAll()
    def extract( a : ArticleStructure) : List[String] = {
      a.article :: a.comments.slice(0,10)
    }
    val corpus = (articles map extract).flatten
    val lda = new LDAWrap(corpus)

    val testSample = articles.tail.head
    val article = testSample.article
  }


}
