package relevance

import java.io.{File, PrintWriter}

import edu.umass.ciir.ResourcePath
import org.scalatest.FunSuite
import org.umass.ciir.dataset.GuardianDataSet.{ArticleStructure, Tool}
import org.umass.ciir.feature.{SimpleTokenzier, TokenizerForTopic}
import org.umass.ciir.query.{BM25, BigramGenerator, Document, WikiTitleQuery}
import org.umass.ciir.miscLib._

import scala.io.Source.fromFile

class relevanceTest extends FunSuite{
  test("WikiTitle test"){
    val wtq = new WikiTitleQuery
    wtq.parsedTitle foreach println
  }
  test("base"){
    val tokenizer = new TokenizerForTopic()
    val tokenizerS = new SimpleTokenzier()
    val bm25 = new BM25(tokenizerS)

    val wtq = time(new WikiTitleQuery, "Loading title query..")
    val querys = time(wtq.parsedTitle.slice(0,100000) map (q => new bm25.Query(q)), "Parsing query's")
    val articles : List[ArticleStructure] = time(Tool.getAll(), "Loading articles..")

    val f = new PrintWriter(new File("RelevanceResult.txt"))

    articles foreach { a =>
      val doc = new Document(a, tokenizerS)
      val topEntry = topKBy(querys,1000)(bm25.score(_, doc)).filter(_._2 > -1000)
      topEntry.foreach { p =>
        val query: bm25.Query = p._1
        val queryString : String = query.query.mkString(" ")
        f.write("%s\t%s\t%f\n".format(a.id, queryString, p._2))
      }
    }
    f.close()
  }

  test("Single word") {
    val tokenizerS = new SimpleTokenzier()
    val bm25 = new BM25(tokenizerS)
    val articlesRaw: List[ArticleStructure] = time(Tool.getAll(), "Loading articles..")
    val articles = articlesRaw.dropRight(50)

    val vocaList: List[Set[String]] = (articles map { a =>
      tokenizerS.TokenizerNormalizeStemmer(a.title + "\n" + a.article).toSet
    })
    val vocaArr = vocaList.toArray
    val vocaCount = count(vocaList.flatMap(x => x.toList))

    def isTag(word:String) : Boolean = word.contains("<") ||  word.contains(">") || word.contains("=")
    val stopwords = (fromFile(ResourcePath.stopwords).getLines() map (_.trim)).toSet
    println(stopwords.head)
    assert(stopwords.contains("a"))
    val threshold = 3
    val voca: Set[String] = vocaList.flatten.toSet filter (vocaCount(_) >= threshold) filterNot (stopwords.contains(_)) filterNot (isTag(_))


    println("Total of %d active words".format(voca.size))
    val querys = voca map (q => new bm25.Query(List(q)))

    val f = new PrintWriter(new File("output\\RelevanceWord.txt"))

    articles foreach { a =>
      val doc = new Document(a, tokenizerS)
      val topEntry = topKBy(querys.toList, 1000)(bm25.score(_, doc)).filter(_._2 > -1000)
      topEntry.foreach { p =>
        val query: bm25.Query = p._1
        val queryString: String = query.query.mkString(" ")
        f.write("%s\t%s\t%f\n".format(a.id, queryString, p._2))
      }
    }
    f.close()

    val appear: Map[String, List[Int]] = {
      voca map { word =>
        val l: IndexedSeq[Int] = for (
          i <- 0 until vocaList.size
          if (vocaArr(i).contains(word))
        ) yield i
        (word, l.toList)
      }
    }.toMap

    val corpusSize = articles.size
    val fc = new PrintWriter(new File("output\\correlations.txt"))
    voca.toList.combinations(2) foreach { l =>
      val w1 = l.head
      val w2 = l.tail.head
      val independence : Double = {
        val common = appear(w1).intersect(appear(w2))
        if (common.size == 0)
          0
        else{
          val pa: Double = appear(w1).size.toDouble / corpusSize
          val pb: Double = appear(w2).size.toDouble / corpusSize
          val pcommon : Double = common.size.toDouble / corpusSize
          pa*pb / pcommon
        }
      }
      fc.write("%s\t%s\t%f\n".format(w1,w2,independence))
    }
    fc.close()

  }

  private def genRelevanceCorpus(tokenizerS: SimpleTokenzier, articles: List[ArticleStructure], bigramQuery: List[List[String]]) = {
    val bm25 = new BM25(tokenizerS)
    def isTag(word: String): Boolean = word.contains("<") || word.contains(">") || word.contains("=")

    val stopwords = (fromFile(ResourcePath.stopwords).getLines() map (_.trim)).toSet

    val threshold = 3

    val vocaList: List[Set[String]] = (articles map { a =>
      tokenizerS.TokenizerNormalizeStemmer(a.title + "\n" + a.article).toSet
    })


    val vocaCount = count(vocaList.flatMap(x => x.toList))
    val vocaArr = vocaList.toArray

    val voca: Set[String] = vocaList.flatten.toSet filter (vocaCount(_) >= threshold) filterNot (stopwords.contains(_)) filterNot (isTag(_))

    val appear: Map[String, List[Int]] = {
      voca map { word =>
        val l: IndexedSeq[Int] = for (
          i <- 0 until vocaList.size
          if (vocaArr(i).contains(word))
        ) yield i
        (word, l.toList)
      }
    }.toMap

    def valid_query(query: List[String]): Boolean = {
      // 1. all stop words
      // 2. do not appear in more than 2 documents
      if (query.forall(stopwords.contains(_)))
        false
      else if (query.forall(voca.contains(_))) {
        val postings = query map appear

        def overlap(l1: List[Int], l2: List[Int]): List[Int] = l1.intersect(l2)

        val commonPostings = postings.foldRight(postings.head)(overlap)
        commonPostings.size >= 3
      }
      else
        false
    }

    val existingBigramQuery = (bigramQuery filter valid_query).toSet.toList
    println("valid query : %d".format(existingBigramQuery.size))

    val fc = new PrintWriter(new File("output\\bigrams.txt"))

    existingBigramQuery foreach { query =>
      fc.write(query.mkString(" ") + "\n")
    }
    fc.close()

    val querys = existingBigramQuery map (new bm25.Query(_))

    val f = new PrintWriter(new File("output\\RelevanceBigram.txt"))

    articles foreach { a =>
      val doc = new Document(a, tokenizerS)
      val topEntry = topKBy(querys, 1000)(bm25.score(_, doc)).filter(_._2 > -1000)
      topEntry.foreach { p =>
        val query: bm25.Query = p._1
        val queryString: String = query.query.mkString(" ")
        f.write("%s\t%s\t%f\n".format(a.id, queryString, p._2))
      }
    }
    f.close()
  }


  test("bigram wiki"){
    val tokenizerS = new SimpleTokenzier()

    val articlesRaw: List[ArticleStructure] = time(Tool.getAll(), "Loading articles..")
    val articles = articlesRaw.dropRight(50)

    val wtq = time(new WikiTitleQuery, "Loading title query..")
    println("Total of %d query".format( wtq.parsedTitle.size))

    val bigramQuery = wtq.parsedTitle.filter( _.size <= 2) map tokenizerS.stemNormalize
    println("Length<=2 : %d query".format( bigramQuery.size))

    genRelevanceCorpus(tokenizerS, articles, bigramQuery)

  }

  test("bigram from corpus"){
    val tokenizerS = new SimpleTokenzier()

    val articlesRaw: List[ArticleStructure] = time(Tool.getAll(), "Loading articles..")
    val articles = articlesRaw.dropRight(50)

    val bigramQuery = (fromFile("output\\guardianBigram.txt").getLines() map (_.split(" ").toList)).toList
    println("Length<=2 : %d query".format( bigramQuery.size))

    genRelevanceCorpus(tokenizerS, articles, bigramQuery)
  }

  test("bigram generations") {
    val tokenizerS = new SimpleTokenzier()
    val bm25 = new BM25(tokenizerS)

    val articlesRaw: List[ArticleStructure] = time(Tool.getAll(), "Loading articles..")
    val articles = articlesRaw.dropRight(50)
    val bigramGenerator = new BigramGenerator(tokenizerS)
    val generated = bigramGenerator.gen(articles)

    val f = new PrintWriter(new File("output\\Bigrams.txt"))
    generated foreach { bigram =>
      f.write(bigram.mkString(" ") + "\n")
    }
    f.close()

  }
}
