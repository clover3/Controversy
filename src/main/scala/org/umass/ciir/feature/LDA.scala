package org.umass.ciir.feature


import scala.collection.JavaConverters._
import java.util
import java.util.regex.Pattern

import cc.mallet.types._
import cc.mallet.pipe._
import cc.mallet.pipe.iterator._
import cc.mallet.pipe.iterator.ArrayIterator
import cc.mallet.topics.ParallelTopicModel
import cc.mallet.types.InstanceList


class LDAWrap (corpus : List[String], val numTopics : Int ){
  private val pipeList = new util.ArrayList[Pipe]

  // Pipes: lowercase, tokenize, remove stopwords, map to features
  pipeList.add(new CharSequenceLowercase)
  pipeList.add(new CharSequence2TokenSequence(Pattern.compile("\\p{L}[\\p{L}\\p{P}]+\\p{L}")))
  pipeList.add(new TokenSequenceRemoveStopwords(false))
  pipeList.add(new TokenSequence2FeatureSequence)

  private val instances = new InstanceList(new SerialPipes(pipeList))
  private val itr = new ArrayIterator(corpus.asJava)
  instances.addThruPipe(itr)
  val model = new ParallelTopicModel(numTopics, 1.0, 0.01)

  model.addInstances(instances)

  // Use two parallel samplers, which each look at one half the corpus and combine
  //  statistics after every iteration.

  // Run the model for 50 iterations and stop (this is for testing only,
  //  for real applications, use 1000 to 2000 iterations)
  model.setNumIterations(1000)
  model.estimate()
  val inferencer = model.getInferencer()

  private val estimator = model.getProbEstimator()

  val wordsP = model.getSortedWords
  val words : Alphabet = model.alphabet
  def convert(set: util.TreeSet[IDSorter])(id : Int) : Double = {
    val idArray : Array[IDSorter] = set.asScala.toArray
    val mapObj = (idArray map ( x => (x.getID(), x.getWeight()))).toMap
    val default = 0
    if(mapObj.contains(id))
      mapObj(id)
    else
      default
  }

  private val topics : IndexedSeq[util.TreeSet[IDSorter]] = Range(0,numTopics) map ((wordsP.get(_)))
  private val arrIDSorter : IndexedSeq[Int => Double] = topics map convert

  def getTopic(topicId : Int) : (Int=>Double) = arrIDSorter(topicId)

  // Probability that each candidate is generated from source's topic , normalized by length of sentence
  def estimate(source: String, candidate : List[String]) : IndexedSeq[Double] = {

    val pipeList = new util.ArrayList[Pipe]

    // Pipes: lowercase, tokenize, remove stopwords, map to features
    pipeList.add(new CharSequenceLowercase)
    pipeList.add(new CharSequence2TokenSequence(Pattern.compile("\\p{L}[\\p{L}\\p{P}]+\\p{L}")))
    pipeList.add(new TokenSequenceRemoveStopwords(false))
    pipeList.add(new TokenSequence2FeatureSequence)

    val instances = new InstanceList(new SerialPipes(pipeList))
    val items : java.util.List[String] = (source::candidate).asJava
    val itr = new ArrayIterator(items)
    instances.addThruPipe(itr)
    val sourceTopic : Array[Double] = inferencer.getSampledDistribution(instances.get(0), 10, 1, 5)

    def globalLikelihood(rawSentence : String) : Double = ???

    def LDALikelihood(candidate : Instance) : Double = {
      def score(token:Int) : Double = {
        val logProb = for(
          i <- 0 until numTopics
        ) yield ( getTopic(i)(token) * sourceTopic(i))
        logProb.sum
      }
      val tokens : FeatureSequence= candidate.getData().asInstanceOf[FeatureSequence]
      val items : IndexedSeq[Int] = for(
        i <- 0 until tokens.getLength()
      ) yield words.lookupIndex(tokens.get(i))

      val scoreArr = items map score
      scoreArr.sum / scoreArr.size
    }

    val scores = for(
      i <- 1 until items.size()
    ) yield LDALikelihood(instances.get(i))
    scores
  }
}
