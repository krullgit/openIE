package de.tu_berlin.dima.code

import java.io.{File, PrintWriter}
import java.nio.charset.Charset
import java.util.Properties

import com.google.common.io.Files
import edu.stanford.nlp.ling.CoreAnnotations.{PartOfSpeechAnnotation, SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap


object POS {

  // METHODS

  ////////////
  // 2 functions:
  // 1. function "group" annotate sentences (from file) with POS tags and group them (and println)to see how often single pattern occur
  // 2. function "save" annotate sentences (from file) with POS tags and save them to a file
  // Instruction: 1. run ssplit to get file
  ////////////

  def annotatePos(filename:String, function:String) = {
    //val text = Source.fromFile(filename).getLines.mkString
    val props: Properties = new Properties() // set properties for annotator
    props.put("annotators", "tokenize, ssplit,pos") // set properties

    val pipeline: StanfordCoreNLP = new StanfordCoreNLP(props) // annotate file

    // read some text from a file - Uncomment this and comment the val text = "Quick...." below to load from a file
    val inputFile: File = new File(filename+"ssplit.txt") // read from file
    val text: String = Files.toString(inputFile, Charset.forName("UTF-8"))

    if(function == "group"){
      ListMap(text.split("\n")
        .map(getPOS(_)) // get POS tags
        .map(_.mkString(" ")) // make List of POS tags to String
        .groupBy(pos => pos) // groupby POS Tag strings
        .mapValues(_.size) // count occurrences
        .toSeq.sortBy(_._2): _*) // order them
        .foreach(println(_))

    }else if (function == "save"){
      new PrintWriter(s"${filename}ssplit_POS.txt") { // open new file
        text.split("\n")
          .map(getPOS(_)) // get POS tags
          .map(_.mkString(" ")) // make List of POS tags to String
          .foreach(x => write(x+"\n"))
        close // close file
      }
    }

    def getPOS(sentence:String): List[(String,String)] ={ // get POS tags per sentence
      val document: Annotation = new Annotation(sentence)
      pipeline.annotate(document) // annotate
      val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).asScala.toList
      for {
        sentence: CoreMap <- sentences
        token: CoreLabel <- sentence.get(classOf[TokensAnnotation]).asScala.toList
        pos: String = token.get(classOf[PartOfSpeechAnnotation])

      } yield (pos, token.originalText()) // return List of POS tags
    }
  }


  // MAIN
  def main(args: Array[String]): Unit = {
    ////////////
    // 2 functions:
    // 1. function "group" annotate sentences (from file) with POS tags and group them (and println)to see how often single pattern occur
    // 2. function "save" annotate sentences (from file) with POS tags and save them to a file
    // Instruction: 1. run ssplit to get file 2. run this with "save"
    ////////////
    annotatePos("reviews24740","save")
  }
}
