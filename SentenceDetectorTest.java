import java.io.*;
import opennlp.tools.sentdetect.*;
import opennlp.tools.util.Span;

//javac -cp /Users/blahiri/openNLP/apache-opennlp-1.5.3/lib/opennlp-tools-1.5.3.jar SentenceDetectorTest.java
//java -cp /Users/blahiri/openNLP/apache-opennlp-1.5.3/lib/opennlp-tools-1.5.3.jar:. SentenceDetectorTest

class SentenceDetectorTest {
  public static void main(String[] args) {
    InputStream modelIn = null;
    try {
     modelIn = new FileInputStream("en-sent.bin");
     SentenceModel model = new SentenceModel(modelIn);
     SentenceDetectorME sentenceDetector = new SentenceDetectorME(model);
     String sentences[] = sentenceDetector.sentDetect("  First sentence. Second sentence. ");
     //Span sentences[] = sentenceDetector.sentPosDetect("  First sentence. Second sentence. ");
     int len = sentences.length;
     for (int i = 0; i < len; i++)
     {
       System.out.println(sentences[i]);
     }
    }
    catch (IOException e) {
     e.printStackTrace();
    }
    finally {
      if (modelIn != null) {
       try {
        modelIn.close();
       }
       catch (IOException e) {
     }
   }
  }
 }
}
