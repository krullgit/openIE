package de.tu_berlin.dima.code

import java.io.{FileInputStream, FileNotFoundException, InputStream}
import java.util.Properties

import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.http.HttpClient
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.process.DocumentPreprocessor
import opennlp.tools.chunker.{ChunkerME, ChunkerModel}

import scala.concurrent.duration.Duration
import scala.io.Source

object main {

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  // load rest
  // - - - - - - - - - - - - - - - - - - - - - - - - -

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
  println("pipelinePos")
  val propsPos: Properties = new Properties() // set properties for annotator
  propsPos.put("annotators", "tokenize, ssplit,pos") // set properties
  propsPos.put("pos.model", "data/english-left3words-distsim.tagger")
  val pipelinePos: StanfordCoreNLP = new StanfordCoreNLP(propsPos) // annotate file

  println("pipelineNER")
  val propsNER: Properties = new Properties() // set properties for annotator
  propsNER.put("annotators", "tokenize, ssplit, pos, lemma, ner, regexner")
  propsNER.put("regexner.mapping", "data/jg-regexner.txt")
  propsNER.put("pos.model", "data/english-left3words-distsim.tagger")
  val pipelineNER: StanfordCoreNLP = new StanfordCoreNLP(propsNER) // annotate file

  println("pipelineSplit")
  val propsSplit: Properties = new Properties()
  propsSplit.put("annotators", "tokenize, ssplit")
  propsSplit.put("pos.model", "data/english-left3words-distsim.tagger")
  val pipelineSplit: StanfordCoreNLP = new StanfordCoreNLP(propsSplit)
  println("READY LOADING STANFORD PARSER")
  /*
    println("LOADING STANFORD PARSER 4")
    val propsDep: Properties = new Properties()
    propsDep.put("annotators", "tokenize,ssplit,pos,depparse")
    propsDep.put("pos.model", "data/english-left3words-distsim.tagger")
    val pipelineDep: StanfordCoreNLP = new StanfordCoreNLP(propsDep)
    println("READY LOADING STANFORD PARSER")
  */

  def main(args: Array[String]): Unit = {






    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // parameter
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    val step: String = "1"
    // 1 = saveSentences
    // 2 = parseExtractions
    // 3 = test chunking
    // 4 = test openIE
    val wordsLowerBorder = 3;
    val wordsUpperBound = 40;
    val numberOfProducts: Int = 10000
    //println("We could fetch theoretically "+iterator.length+" product IDs") // 400000
    val maxRedundantChunkPattern: Int = 3
    val category: String = "tools"

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // get current file name
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    val extractionObject = new getExtractions(client,chunker,pipelinePos,pipelineNER,pipelineSplit,pipelineSplit)
    var reviewName = ""
    def getCurrentFileName(): Unit = {
      (0 to 1000).foreach(index => {
        try {
          (Source.fromFile("data/review" + index + "_ssplit.txt"))
          //println("review" + index + " gibt es schon.")
        } catch {
          case e: FileNotFoundException => reviewName = "review" + index; return
        }
      }
      )
    }
    getCurrentFileName()
    println("reviewName is: "+reviewName)
    println("YOU CHOSE STEP: "+step)

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // get and save list of reviews
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    if (step == "1") {
      // We fetch the product IDs of a special product category e.g. "tools"
      println("We fetch the product IDs of a special product category e.g. \"tools\"")
      val listOfProducts = extractionObject.getListOfProducts(category, numberOfProducts)
      println("N U M B E R  O F  P R O D U C T S: " + listOfProducts.size)
      // We fetch the products reviews according to the IDs we got
      println("We fetch the products reviews according to the IDs we got")
      val vectorOfReviews = extractionObject.getVectorOfRawReviews(reviewName, listOfProducts)
      println("S I Z E - R E V I E W S: " + vectorOfReviews.size)
      // We apply sentence splitting on each of the reviews
      println("We apply sentence splitting on each of the reviews")
      val vectorOfSentences = extractionObject.ssplit(vectorOfReviews)
      println("S I Z E - S E N T E N C E S : " + vectorOfSentences.size)
      // We filter and store sentences, to run an open IE system on in afterwards
      println("We filter and store sentences, to run an open IE system on in afterwards")
      extractionObject.estimateRecall(reviewName, vectorOfSentences ,maxRedundantChunkPattern ,wordsLowerBorder ,wordsUpperBound)
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // parse extractions and index them into elastic
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    else if (step == "2") {
      // change filename
      // change elasticsearch index at line 643
      extractionObject.parseExtractions("out3.txt")
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // test chunker
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    else if (step == "3") {
      while (true) {
        print("Sentence: ")
        val sentence: String = scala.io.StdIn.readLine()
        val sentWithPos: Vector[(String, String)] = extractionObject.getPOS(sentence)
        println("sentWithPos: "+sentWithPos)
        val sentChunked: Vector[String] = extractionObject.chunking(sentWithPos.map(x => x._2).toArray, sentWithPos.map(x => x._1).toArray).toVector
        println(sentChunked.zip(sentWithPos.map(x => x._2)))
      }}

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // test system
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    else if (step == "4") {

      extractionObject.testExtraction
    }
    println("DONE")
    client.close()
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  // close elastic client
  // - - - - - - - - - - - - - - - - - - - - - - - - -

}
