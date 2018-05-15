package de.tu_berlin.dima.code

import java.io.{FileInputStream, FileNotFoundException, InputStream}
import java.util.Properties

import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.http.HttpClient
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import opennlp.tools.chunker.{ChunkerME, ChunkerModel}

import scala.concurrent.duration.Duration
import scala.io.Source

object main {

  val client: HttpClient = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
  implicit val timeout = Duration(20, "seconds") // is the timeout for the SearchIterator.hits method

  var modelIn: InputStream = null;
  var model: ChunkerModel = null;
  try {
    modelIn = new FileInputStream("data/en-chunker.bin")
  }
  model = new ChunkerModel(modelIn)
  val chunker: ChunkerME = new ChunkerME(model)

  println("LOADING STANFORD PARSER")
  val propsPos: Properties = new Properties() // set properties for annotator
  propsPos.put("annotators", "tokenize, ssplit,pos") // set properties
  val pipelinePos: StanfordCoreNLP = new StanfordCoreNLP(propsSplit) // annotate file

  val propsNER: Properties = new Properties() // set properties for annotator
  propsNER.put("annotators", "tokenize, ssplit, pos, lemma, ner, regexner")
  propsNER.put("regexner.mapping", "data/jg-regexner.txt")
  val pipelineNER: StanfordCoreNLP = new StanfordCoreNLP(propsNER) // annotate file

  val propsSplit: Properties = new Properties()
  propsSplit.put("annotators", "tokenize, ssplit")
  val pipelineSplit: StanfordCoreNLP = new StanfordCoreNLP(propsSplit)
  println("READY LOADING STANFORD PARSER")


  def main(args: Array[String]): Unit = {

    val extractionObject = new getExtractions(client,chunker,pipelinePos,pipelineNER,pipelineSplit)
    val numberOfProducts: Int = 1000

    var reviewName = ""

    def getCurrentFileName(): Unit = {
      (0 to 1000).foreach(index => {
        try {
          (Source.fromFile("data/review" + index + "_ssplit.txt")); println("review" + index + " gibt es schon.")
        } catch {
          case e: FileNotFoundException => reviewName = "review" + index; return
        }
      }
      )
    }

    getCurrentFileName()

    println("Step 1 or 2?")
    val step: String = "3" //scala.io.StdIn.readLine()

    if (step == "1") {
      val listOfProducts = extractionObject.getListOfProducts("tools", numberOfProducts)
      println("N U M B E R  O F  P R O D U C T S: " + listOfProducts.size)
      val vectorOfReviews = extractionObject.saveAmazonReviewsInAFile(reviewName, listOfProducts)
      println("S I Z E - R E V I E W S: " + vectorOfReviews.size)
      val vectorOfSentences = extractionObject.ssplit(vectorOfReviews)
      //println("S I Z E - S E N T E N C E S  A L L: " + vectorOfSentences.size)
      extractionObject.estimateRecall(reviewName, vectorOfSentences)
    } else if (step == "2") {
      // change filename
      // change elasticsearch index at line 643
      extractionObject.parseExtractions("out4.txt")
    } else if (step == "3") {
      extractionObject.testExtraction
    }
    println("DONE")
  }

}
