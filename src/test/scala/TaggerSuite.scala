import java.io.StringBufferInputStream

import edu.stanford.nlp.tagger.maxent.MaxentTagger
import org.scalatest.FunSuite
import org.umass.ciir.feature.POSCounter

class TaggerSuite extends FunSuite {


  test("simple pos"){
    import edu.stanford.nlp.tagger.maxent.MaxentTagger
    val tagger = new MaxentTagger("models/english-left3words-distsim.tagger")

    // The sample string
    val sample = "This is a sample text"
    val longSample = "The basic download is a 24 MB zipped file with support for tagging English. The full download is a 124 MB zipped file, which includes additional English models and trained models for Arabic, Chinese, French, Spanish, and German. In both cases most of the file size is due to the trained model files. The only difference between the two downloads is the number of trained models included. If you unpack the tar file, you should have everything needed. This software provides a GUI demo, a command-line interface, and an API. Simple scripts are included to invoke the tagger. For more information on use, see the included README.txt.\n"
    val tricky = "This_is tricky NN"


    // The tagged string

    val r = tagger.tagString(tricky)

    // Output the result
    System.out.println(r)
  }

  test("POS Counter test"){
    val counter = new POSCounter()
    val longSample = "The basic download is a 24 MB zipped file with support for tagging English. The full download is a 124 MB zipped file, which includes additional English models and trained models for Arabic, Chinese, French, Spanish, and German. In both cases most of the file size is due to the trained model files. The only difference between the two downloads is the number of trained models included. If you unpack the tar file, you should have everything needed. This software provides a GUI demo, a command-line interface, and an API. Simple scripts are included to invoke the tagger. For more information on use, see the included README.txt.\n"
    val article = "Um, Uh, Universities should make greater efforts to spot talent among disadvantaged students and place special emphasis on encouraging white working-class men in order to widen access to higher education, according to a new report backed by British universities.\n\nThe report – produced by the sector at the request of the government – found that “socio-economic disadvantage has more persistent and far-reaching impact on access to and outcomes from higher education” than any other single cause.\n\nBut the study by Universities UK’s social mobility action group offered few concrete suggestions on how to overcome the effects of disadvantage, and instead called for further data collection, analysis and sharing of best practice between institutions.\n\nNicola Dandridge, the chief executive of Universities UK who chaired the advisory group, said: “The evidence provides a stark reminder of the work that still needs to be done to improve social mobility. Disadvantage is deeply entrenched in our society, and there are no quick and easy answers.\n\n“Universities are absolutely committed to promoting social mobility and are undertaking extensive and ambitious work. Our report concludes that we need to carry on doing this work, but with more evaluation, more focus on advice and guidance to students, and better collaboration with schools and employers, and with government.”\n\nThe report painted a stark picture of the gaps in access to higher education, with 18-year-olds from the most advantaged groups more than six times more likely to attend highly selective institutions than those from disadvantaged backgrounds.\n\nIt also noted the “unexplained” 15 percentage point gap between the proportion of white graduates gaining a first or upper second degree and those from black and minority ethnic backgrounds.\n\nLes Ebdon, a member of the advisory group and the government’s director of fair access to higher education, said he was encouraged by the report’s emphasis on the role of universities in widening access.\n\n“But there is still much work to do. Too many talented people are still missing out on the life-changing benefits of higher education,” Ebdon said.\n\n\nUniversity of Oxford rebuts Cameron's claims over student diversity\n Read more\nAmong the report’s recommendations was that universities make “greater use of contextual data to inform offer making”, meaning applicants’ social and economic family backgrounds be used to adjust admissions offers, and to identify groups or regions that find it more difficult to gain access.\n\nCombating the effects of disadvantage and poor schooling, according to the report, “may also require wider use of contextual admissions processes in which universities identify an applicant’s potential as well as their prior attainment in determining admissions”.\n\nWhile many universities already use some forms of contextual data in admissions decisions, its widespread use has been resisted especially among selective universities.\n\nThe report also concluded that “the evidence suggests there should be a particular focus on access for white working-class men”, although it noted that access rates for white working-class women and those from mixed race backgrounds were little better.\n\n“Ensuring sustained, targeted outreach work that starts at primary school is critical if we are to improve access for this group, along with ensuring that pupils are informed about their options and know why higher education is relevant to their future plans,” the authors, which included several vice-chancellors and senior academics, concluded.\n\nJo Johnson, the universities minister, said: “We are seeing record numbers of disadvantaged young people going to university and benefiting from the real opportunities that our world-class universities can offer.\n\n“But, as this report makes clear, there is still more to do. That is why I welcome this important piece of work from Universities UK and also why we are legislating for a new transparency duty which will place a clear requirement on all universities to release more information about their admissions process and real incentives on all institutions to go further and faster to promote social mobility.”\n\nThe social mobility advisory group was established last year after a request by Johnson. Earlier this year the then prime minister, David Cameron, was highly critical of university admissions policies, saying: “Too many in our country are held back – often invisibly – because of their background or the colour of their skin.”\n"
    counter.count(article) foreach println
    println(counter.getVector(article))
  }

}
