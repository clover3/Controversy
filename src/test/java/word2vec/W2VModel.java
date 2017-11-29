package word2vec;

import java.util.List;

import org.insight.wordspace.W2vSpace;
import org.insight.wordspace.util.WordSim;
import org.junit.Test;

public class W2VModel {

  @Test
  public void test() {
    long startTime = System.nanoTime();

    for (int j = 0; j < 1; j++) {
      W2vSpace w2v =
          W2vSpace
              .load("C:\\work\\Data\\GoogleNews-vectors-negative300.bin\\GoogleNews-vectors-negative300.bin");

      List<WordSim> sims = w2v.knn(w2v.vector("democrats"), 10, w2v.f.wordsOnly(w2v.f.removeWords.with("democrat", "democrats")));

      w2v.printSims("test ", sims);

    }

    long estimatedTime = System.nanoTime() - startTime;
    float secs = estimatedTime / 1000000000.0F;
    System.out.println("TOTAL EXECUTION TIME: " + secs);
  }

}
