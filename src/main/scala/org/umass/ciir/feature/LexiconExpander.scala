package org.umass.ciir.feature

import org.insight.wordspace.W2vSpace
import org.jblas.FloatMatrix

import scala.collection.JavaConverters._


class LexiconExpander(seedsA : List[String], seedsB : List[String], targetSize: Int)
{
    private object eval{

      println("Loading w2v..")
      val w2v = W2vSpace.load("C:\\work\\Data\\GoogleNews-vectors-negative300.bin\\GoogleNews-vectors-negative300.bin")
      println("Loaded w2v")
      val vectorsA : List[FloatMatrix] = seedsA map w2v.vector
      val vectorsB : List[FloatMatrix] = seedsB map w2v.vector

      def mean(inputs: List[FloatMatrix]) : FloatMatrix = {
        def addFM(a : FloatMatrix, b: FloatMatrix) : FloatMatrix = a.add(b)
        val sum = inputs.tail.foldRight(inputs.head)(addFM)
        sum.divi(inputs.length)
      }
      val centroidA = mean(vectorsA)
      val centroidB = mean(vectorsB)

      def score(synonyms: List[FloatMatrix], antonyms: List[FloatMatrix])(target: FloatMatrix): Double = {
        val synDists = synonyms map (x => w2v.cosineSimilarity(target, x))
        val antDists = antonyms map (x => w2v.cosineSimilarity(target, x))
        synDists.sum
      }


      val candidateA : List[String] = asScalaBuffer(w2v.knnWords(centroidA, targetSize * 4)).toList.filterNot(x=> x.contains('_'))
      val candidateB = asScalaBuffer(w2v.knnWords(centroidB, targetSize * 4)).toList.filterNot(x=> x.contains('_'))

      val common : Set[String] = (candidateA++seedsA).intersect(candidateB++seedsB).toSet

      val candidateScoreA =  candidateA map (x => (x, score(vectorsA, vectorsB)(w2v.vector(x))))
      val candidateScoreB =  candidateB map (x => (x, score(vectorsB, vectorsA)(w2v.vector(x))))

      val candidateRankA = candidateScoreA.sortBy((_._2)).unzip._1 zip Range(0,candidateA.size)
      val candidateRankB = candidateScoreB.sortBy((_._2)).unzip._1 zip Range(0,candidateB.size)


      val rankMapA = candidateRankA.toMap
      val rankMapB = candidateRankB.toMap

      def filterA(pair: (String,Double)) : Boolean = {
        val word = pair._1
        if(common contains word)
          rankMapA(word) < rankMapB(word)
        else
          false
      }
      def filterB(pair: (String,Double)) : Boolean = {
        val word = pair._1
        if(common contains word)
          rankMapB(word) < rankMapA(word)
        else
          false
      }

      val candidateExclusiveA = candidateScoreA filterNot filterA
      val candidateExclusiveB = candidateScoreB filterNot filterB

      val resultA = (candidateExclusiveA sortBy (x => -x._2)).slice(0,targetSize).unzip._1
      val resultB = (candidateExclusiveB sortBy (x => -x._2)).slice(0,targetSize).unzip._1
    }

  val aList = eval.resultA
  val bList = eval.resultB
}



