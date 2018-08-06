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
import de.tu_berlin.dima.code.getExtractions

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

  def getpipelinePos: StanfordCoreNLP ={
    println("pipelinePos")
    val propsPos: Properties = new Properties() // set properties for annotator
    propsPos.put("annotators", "tokenize, ssplit,pos") // set properties
    propsPos.put("pos.model", "data/english-left3words-distsim.tagger")
    //propsPos.put("pos.model", "data/english-bidirectional-distsim.tagger")
    new StanfordCoreNLP(propsPos) // annotate file
  }

  def getpipelineNER: StanfordCoreNLP = {
    println("pipelineNER")
    val propsNER: Properties = new Properties() // set properties for annotator
    propsNER.put("annotators", "tokenize, ssplit, pos, lemma, ner, regexner")
    propsNER.put("regexner.mapping", "data/jg-regexner.txt")
    propsNER.put("pos.model", "data/english-left3words-distsim.tagger")
    //propsNER.put("pos.model", "data/english-bidirectional-distsim.tagger")
    new StanfordCoreNLP(propsNER) // annotate file
  }
  def getpipelineSplit: StanfordCoreNLP = {
    println("pipelineSplit")
    val propsSplit: Properties = new Properties()
    propsSplit.put("annotators", "tokenize, ssplit")
    propsSplit.put("pos.model", "data/english-left3words-distsim.tagger")
    //propsSplit.put("pos.model", "data/english-bidirectional-distsim.tagger")
    new StanfordCoreNLP(propsSplit)
  }
  def getpipelineDep: StanfordCoreNLP = {
    println("pipelineDep")
    val propsDep: Properties = new Properties()
    propsDep.put("annotators", "tokenize,ssplit,pos,depparse")
    propsDep.put("pos.model", "data/english-left3words-distsim.tagger")
    new StanfordCoreNLP(propsDep)
  }

  def main(args: Array[String]): Unit = {


    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // parameter
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    val step: String = "6"
    // 1 = saveSentences
    // 2 = parseExtractions
    // 3 = test chunking
    // 4 = test openIE

    // 1
    val wordsLowerBorder = 1;
    val wordsUpperBound = 100;
    val numberOfProducts: Int = 100 //println("We could fetch theoretically "+iterator.length+" product IDs") // 400000
    val maxRedundantChunkPattern: Int = 3
    val category: String = "styling"

    // 2
    val fileNameForParsingExtractions = "out5.txt"
    val ElasticIndexName = "amazon_extractions_5"

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // get current file name
    // - - - - - - - - - - - - - - - - - - - - - - - - -


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
    println("reviewName is: " + reviewName)
    println("YOU CHOSE STEP: " + step)

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // get and save list of reviews
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    if (step == "1") {
      val pipelinePos = getpipelinePos
      val pipelineNER = getpipelineNER
      val pipelineSplit = getpipelineSplit
      val extractionObject = new getExtractions(client, pipelinePos, pipelineNER, pipelineSplit, pipelineSplit, model: ChunkerModel)
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
      extractionObject.storeSentences(reviewName, vectorOfSentences, maxRedundantChunkPattern, wordsLowerBorder, wordsUpperBound)
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // parse extractions and index them into elastic
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    else if (step == "2") {
      val pipelinePos = getpipelinePos
      val extractionObject = new parseExtractions(client, pipelinePos, model: ChunkerModel)
      // change filename
      // change elasticsearch index at line 643

      extractionObject.parseExtractionsController(fileNameForParsingExtractions, ElasticIndexName)
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // test chunker
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    else if (step == "3") {
      val pipelinePos = getpipelinePos
      val pipelineNER = getpipelineNER
      val pipelineSplit = getpipelineSplit
      val extractionObject = new getExtractions(client, pipelinePos, pipelineNER, pipelineSplit, pipelineSplit, model: ChunkerModel)
      while (true) {
        print("Sentence: ")
        val sentence: String = scala.io.StdIn.readLine()
        val sentWithPos: Vector[(String, String)] = extractionObject.getPOS(sentence)
        val token = sentWithPos.map(x => x._2).map(_.toString)
        println("sentWithPos: " + sentWithPos)
        val sentChunked: Vector[String] = extractionObject.chunking(sentWithPos.map(x => x._2).toArray, sentWithPos.map(x => x._1).toArray).toVector

        val chunkedReplacedO = sentChunked.zipWithIndex.map { case (chunk, i) => {
          if (chunk == "O") {
            sentWithPos(i)._1
          } else {
            chunk
          }
        }
        }
        println(chunkedReplacedO.zip(token))

        val chunkedTagsReduced = Vector.newBuilder[String]
        val chunkedSentenceReduced = Vector.newBuilder[String]


        chunkedTagsReduced += chunkedReplacedO(0)
        var chunkedSentenceReducedTMP: String = token(0)

        for (i <- 1 until chunkedReplacedO.size) {
          if (chunkedReplacedO(i)(0).toString != "I") {
            chunkedTagsReduced += chunkedReplacedO(i)
            chunkedSentenceReduced += chunkedSentenceReducedTMP
            chunkedSentenceReducedTMP = ""
            chunkedSentenceReducedTMP += " " + token(i)
          } else {
            chunkedSentenceReducedTMP += " " + token(i)
          }
        }
        println("chunkedTagsReduced: " + chunkedTagsReduced.result().mkString("$"))
        println("chunkedSentenceReduced: ")
        chunkedSentenceReduced.result().map(_.trim).foreach(x => print("[" + x + "]"))
        println("")

      }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // test system
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    else if (step == "4") {
      val pipelinePos = getpipelinePos
      val pipelineNER = getpipelineNER
      val pipelineSplit = getpipelineSplit
      val extractionObject = new getExtractions(client, pipelinePos, pipelineNER, pipelineSplit, pipelineSplit, model: ChunkerModel)

      extractionObject.testExtraction
    }
    // Was is denn das hier?
    else if (step == "5") {
      val pipelinePos = getpipelinePos
      val pipelineNER = getpipelineNER
      val pipelineSplit = getpipelineSplit
      val extractionObject = new getExtractions(client, pipelinePos, pipelineNER, pipelineSplit, pipelineSplit, model: ChunkerModel)
      while (true) {
        print("Sentence: ")
        val sentence: String = scala.io.StdIn.readLine()
        val sentWithPos = extractionObject.getDEP(sentence)
      }
    }
    else if (step == "6") {
      val pipelinePos = getpipelinePos
      val pipelineNER = getpipelineNER
      val pipelineSplit = getpipelineSplit
      val extractionObject = new getExtractions(client, pipelinePos, pipelineNER, pipelineSplit, pipelineSplit, model: ChunkerModel)

      extractionObject.testExtractionFile
    }
    println("DONE")
    client.close()
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  // close elastic client
  // - - - - - - - - - - - - - - - - - - - - - - - - -

}
