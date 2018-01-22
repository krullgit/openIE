package de.tu_berlin.dima.code

import java.io.{File, PrintWriter}
import java.nio.charset.Charset
import java.util.Properties

import com.google.common.io.Files
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConverters._

object ssplit {

  // METHODS

  ////////////////////
  // get a file with all reviews's ssplit
  ////////////////////


  def ssplit(filename:String) = {
    //val text = Source.fromFile(filename).getLines.mkString
    val props: Properties = new Properties()
    props.put("annotators", "tokenize, ssplit")

    val pipeline: StanfordCoreNLP = new StanfordCoreNLP(props)

    // read some text from a file - Uncomment this and comment the val text = "Quick...." below to load from a file
    val inputFile: File = new File(filename+".txt")
    val text: String = Files.toString(inputFile, Charset.forName("UTF-8"))
    // create blank annotator
    val document: Annotation = new Annotation(text)
    // run all Annotator - Tokenizer on this text
    pipeline.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).asScala.toList
    new PrintWriter(s"${filename}ssplit.txt") { // open new file
      (for {
          sentence: CoreMap <- sentences
      } yield (sentence)).map(_.toString).foreach(x => write(x + "\n"))
      close // close file
    }
  }

  // MAIN
  def main(args: Array[String]): Unit = {
    ssplit("reviews24740")
  }
}
