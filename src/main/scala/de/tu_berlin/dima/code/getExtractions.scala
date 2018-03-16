package de.tu_berlin.dima.code


import java.io.{File, FileNotFoundException, PrintWriter}
import java.nio.charset.Charset
import java.util.Properties

import com.google.common.io.Files
import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.http.HttpClient
import com.sksamuel.elastic4s.http.search.{SearchHit, SearchIterator}
import edu.stanford.nlp.ling.CoreAnnotations.{PartOfSpeechAnnotation, SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap

import java.io.{FileInputStream, InputStream}
import opennlp.tools.chunker.ChunkerME

import opennlp.tools.chunker._

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.{Failure, Success, Try}
import opennlp.tools.chunker
import java.io.{FileInputStream, InputStream}
import opennlp.tools.chunker.ChunkerME

import opennlp.tools.chunker._


object getExtractions {

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  var modelIn:InputStream  = null;
  var model:ChunkerModel  = null;
  try {
    modelIn = new FileInputStream("data/en-chunker.bin")
  }
  model = new ChunkerModel(modelIn)
  val chunker = new ChunkerME(model)
  def chunking(sent:Array[String],pos:Array[String]): Array[String] ={
    val tag: Array[String] = chunker.chunk(sent, pos)
    //println(tag.zip(sent).foreach(println(_)))
    tag
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  val props: Properties = new Properties() // set properties for annotator
  props.put("annotators", "tokenize, ssplit,pos") // set properties
  val pipeline: StanfordCoreNLP = new StanfordCoreNLP(props) // annotate file
  def getPOS(sentence: String): Vector[(String, String)] = { // get POS tags per sentence
    val document: Annotation = new Annotation(sentence)
    pipeline.annotate(document) // annotate
    val sentences: Vector[CoreMap] = document.get(classOf[SentencesAnnotation]).asScala.toVector
    for {
      sentence: CoreMap <- sentences
      token: CoreLabel <- sentence.get(classOf[TokensAnnotation]).asScala.toVector
      pos: String = token.get(classOf[PartOfSpeechAnnotation])

    } yield (pos, token.originalText()) // return List of POS tags
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  def getListOfProducts(category: String, numberOfProducts: Int): Vector[String] = {
    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    implicit val timeout = Duration(20, "seconds") // is the timeout for the SearchIterator.hits method
    val listBuilder = Vector.newBuilder[String]


    def hereToServe(): Unit = {
      val iterator: Iterator[SearchHit] = SearchIterator.hits(client, search("amazon_reviews_meta") query fuzzyQuery("categories", category).fuzziness("1") keepAlive (keepAlive = "10m") size 1000) //sourceInclude List("text.string","id"))

      iterator.foreach(searchhit => {
        listBuilder += searchhit.sourceField("asin").toString
        if (listBuilder.result().size > numberOfProducts) return
      })
    }

    hereToServe()

    /*val resp = client.execute {
      search("amazon_reviews_meta" / "doc").keepAlive("1m").size(numberOfProducts) query fuzzyQuery("categories", category).fuzziness("1")
    }.await()
    resp match {
      case Left(failure) => println("We failed " + failure.error)
      case Right(results) => results.result.hits.hits.foreach(x => {
        listBuilder += x.sourceField("asin").toString
      })
    }*/
    client.close()

    listBuilder.result()

  }

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -


  def saveAmazonReviewsInAFile(filename: String, listOfProducts: Vector[String]): Vector[String] = {
    val listOfListsOfProducts = listOfProducts.grouped(100).toList
    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    val listBuilder = Vector.newBuilder[String]
    for (i <- listOfListsOfProducts.indices) {
      println("the reviews of how many poducts we got: " + i * 100)
      val resp = client.execute {
        search("amazon_reviews").keepAlive("1m").size(10000) query termsQuery("asin.keyword", listOfListsOfProducts(i))
      }.await
      resp match {
        case Left(failure) => println("We failed " + failure.error)
        case Right(results) => results.result.hits.hits.foreach(x => {
          listBuilder += x.sourceField("reviewText").toString
        })
      }
    }

    new PrintWriter(s"data/${filename}.txt") { // open new file
      listBuilder.result().foreach(x => write(x + "\n")) // write distinct list to file
      //iLiveToServe.distinct.sorted.map(ssplit(_)).flatten.foreach(x => write(x + "\n")) // write distinct list to file
      close // close file
    }



    client.close() // close HttpClient
    listBuilder.result()
    //listBuilder.result().mkString("\n")
  }


  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -


  def ssplit(reviews: Vector[String]): Vector[String] = {

    //
    // TRY SENTENCE SPLIT
    //

    val props: Properties = new Properties()
    props.put("annotators", "tokenize, ssplit")
    val pipeline: StanfordCoreNLP = new StanfordCoreNLP(props)

    // create blank annotator
    println("start ssplit")

    val back = Vector.newBuilder[String]
    reviews.foreach(review => {
      val document: Annotation = new Annotation(review)
      // run all Annotator - Tokenizer on this text
      pipeline.annotate(document)
      val sentences: Vector[CoreMap] = document.get(classOf[SentencesAnnotation]).asScala.toVector
      val backTmp: Vector[String] = (for {
        sentence: CoreMap <- sentences
      } yield (sentence)).map(_.toString)
      backTmp.foreach(x=>back+=x)
      if (back.result().size%1000==0) println("splitted: "+ back.result().size)
    })


    back.result()
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  // 2 functions:
  // 1. function "group" annotate sentences (from file) with POS tags and group them (and println)to see how often single pattern occur
  // 2. function "save" annotate sentences (from file) with POS tags and save them to a file
  // Instruction: 1. run ssplit to get file
  // - - - - - - - - - - - - - - - - - - - - - - - - -


  def annotatePos(filename: String, sentences: Vector[String]) = {
    val wordsLowerBorder = 9;
    val wordsUpperBound = 9;

    //val text = Source.fromFile(filename).getLines.mkString


    ////////////////DEBUG///////////////
    println("start annotatePos")


      var countOne: Double = 0
      var countMoreThanOne: Double = 0

      val sentencesFilteredSpeacialCharacters: Seq[String] = sentences.filter(x=>{
        val hits = Vector.newBuilder[Int]
        hits += x.indexOf("?")
        hits += x.indexOf("(")
        hits += x.indexOf(")")
        //hits += x.indexOf(",")
        hits += x.indexOf(":")
        if(hits.result().max >= 0) false else true
      })

      val sentSplitAtAnd = sentencesFilteredSpeacialCharacters.flatMap(x=>x.split("([ ][a][n][d][ ])").map(x=>x.trim))
      val sentSplitAtBut = sentSplitAtAnd.flatMap(x=>x.split("([ ][b][u][t][ ])").map(x=>x.trim))
      val sentSplitAtKomma = sentSplitAtBut.flatMap(x=>x.split("([,]+)").map(x=>x.trim))


      val sentFilteredSigns = sentSplitAtKomma
        .filter(x => x.size <= 80)
        .map(x=>{
          val listOfIndices = scala.collection.mutable.ArrayBuffer[Int]()
          listOfIndices += x.indexOf(".")
          listOfIndices+=x.indexOf("?")
          listOfIndices+=x.indexOf("!")
          listOfIndices.filter(x=>x<0)
          if(!listOfIndices.filter(x=>x>0).isEmpty){
            x.substring(0,listOfIndices.filter(x=>x>0).min)
          }else{
            x
          }
        })
        .map(x=>x.split(" "))
        .filter(x=>x.size<=wordsUpperBound && x.size >= wordsLowerBorder)
        .map(x=>x.mkString(" "))
      println("S I Z E - S E N T E N C E S  B E T W E E N  " +wordsLowerBorder+ " & " +wordsUpperBound+  "  W O R D S: "+sentFilteredSigns.size)

      new PrintWriter(s"data/${filename}_ssplit.txt") { // open new file
        sentFilteredSigns.foreach(x => write(x + "\n"))
        close // close file
      }

      val sentWithPos: Seq[Vector[(String, String)]] = sentFilteredSigns
        //.map(x=>x.split(" "))
        //.filter(x=>x.size==10)
        //.map(x=>x.mkString(" "))
        .map(getPOS(_)) // get POS tags


      new PrintWriter(s"data/${filename}_pos.txt") { // open new file
        sentWithPos.foreach(x => write(x + "\n"))
        close // close file
      }
      //helper1.foreach(x => listBuilder += x.size)
      //println("size -> occurences: " + listBuilder.result().groupBy(identity).mapValues(_.size))


      //helper01.map(x=>x.map(x=>x._2))
/*      val sentChunked: Seq[Vector[String]] = sentWithPos //
        .map(x=>x.map(x=>x._1)) // get just the POS
        .map(x=>x.map(x=>{
          val replacementV = Vector("VB","VBD","VBG","VBN","VBP","VBZ")
          val replacementN = Vector("NN","NNS","NNP","NNPS","PRP")
          val replacementR = Vector("RB","RBR","RBS")
          val replacementJ = Vector("JJ","JJR","JJS")
          if(replacementV.contains(x)){
            "V"
          }else if(replacementN.contains(x)){
            "N"
          }else if(replacementR.contains(x)){
            "R"
          }else if(replacementJ.contains(x)){
            "J"
          }else{
            x
          }
        }))*/


      println("start chunking")
      val sentChunked: Seq[Array[String]] = sentWithPos.zipWithIndex
        .map{case (vectorOfTuples,i) => {
          if (i%1000==0) println("chunked: "+ i)
          chunking(vectorOfTuples.map(x=>x._2).toArray,vectorOfTuples.map(x=>x._1).toArray)
        }}

      val sentChunkedWithoutI = sentChunked.map(x=>x.filter(x=>x(0).toString!="I")) // I filter the I because I want to reduce every chun to one toke e.g. "big green house"

      new PrintWriter(s"data/${filename}_chunks.txt") { // open new file
        sentChunkedWithoutI.foreach(x => write(x.mkString(" ") + "\n"))
        close // close file
      }

      sentChunkedWithoutI.map(_.mkString(" ")) // make List of POS tags to String
        .groupBy(pos => pos) // groupby POS Tag strings
        .mapValues(_.size) // count occurrences
        .toSeq // order them
        .foreach(x => {
        if (x._2 == 1) {
          countOne += 1
        }
        else if (x._2 > 1) {
          countMoreThanOne += 1
        }
      })
      println("countOne: " + countOne)
      println("countMoreThanOne: " + countMoreThanOne)
      println("So viel Prozent der Pos Kombinationen kommen mindestens doppelt vor: " + (countMoreThanOne / (countOne + countMoreThanOne)) * 100)

  }

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  def parseExtractions(filename: String): List[List[String]] = {
    /*
    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    client.execute { deleteIndex("amazon_extractions_"+filename)}
    client.execute { createIndex("amazon_extractions_"+filename) mappings (
      mapping("doc") as (
        keywordField("pattern"),
        keywordField("extractions")
      ))
    }*/

    println("loadFiles")
    val inputFile1: File = new File(filename) // read from file
    val inputFile3: File = new File(filename + "ssplit_POS.txt") // read from file
    var extr: Vector[String] = Files.toString(inputFile1, Charset.forName("UTF-8")).split("\n\n").toVector
    var pos: Vector[String] = Files.toString(inputFile3, Charset.forName("UTF-8")).split("\n").toVector
    val (text1, text3) = (extr, pos).zipped.toVector.filter(x => x._2.length <= 10).unzip


    // this value lookes like: List((PRP,I), (VBP,like), (PRP,it), (.,.))
    println("listOfListsOfTuplesWithPOSAndWords")
    val listOfListsOfTuplesWithPOSAndWords: Vector[List[(String, String)]] = text3.map(x => x.split("\\s").toList.map(x => {
      val y = x.drop(1).dropRight(1)

      val z = y.split("\\,")
      if (z.size == 2) {
        (z(0), z(1))
      } else if (y.equals(",,,")) {
        (",", ",")
      } else {
        //println("unknown POS: "+y)
        ("", "")
      }
    })).toVector.map(x => x.filter(x => {
      if (x._1 == "," && x._1 == ",") {
        false
      } else {
        true
      }
    })) // filter all (",",",")

    // this value lookes like: List(List(List(Nautica makes quality products.), List(Nautica,  makes,  quality products)), ....
    println("parsedExtractions")
    val parsedExtractions: Vector[List[List[String]]] = text1.map(_.split("\n")).map(_.toList).map(x => x.map(x => {
      val indexFirst = x.indexOf("(")
      val indexLast = x.lastIndexOf(")")
      if (indexFirst == -1 || indexLast == 1) {
        List(x)
      } else {
        x.drop(indexFirst + 1).dropRight(x.size - indexLast).split(";").toList.map(x => x.trim)
      }
    })).toVector

    println("rulesAll")
    val rulesAll = List.newBuilder[List[String]]
    val parsedExtractionsSize = parsedExtractions.size
    for (i <- parsedExtractions.indices) {
      println("rulesAll: " + i + " of " + parsedExtractionsSize)
      val rules = List.newBuilder[String]

      rules += (for (i <- listOfListsOfTuplesWithPOSAndWords(i)) yield {
        i._1
      }).mkString(" ")
      //println(parsedExtractions(i)(0)(0)) // print sentences

      for (lineNumber <- parsedExtractions(i).indices) {
        var rule: String = ""
        if (lineNumber != 0) {
          if (parsedExtractions(i)(lineNumber)(0) == "No extractions found.") {

          } else {

            if (parsedExtractions(i)(lineNumber).size == 3) {
              for (partOfTriple <- parsedExtractions(i)(lineNumber)) {
                partOfTriple.split(" ").zipWithIndex.foreach { case (word, k) => {

                  val occurenceIndices = List.newBuilder[Int]

                  listOfListsOfTuplesWithPOSAndWords(i).zipWithIndex.foreach { case (tuple, j) => if (tuple._2 == word) {
                    occurenceIndices += j
                  }
                  }
                  if (occurenceIndices.result().size > 1) { // we found more than one occurence of the current word in the sentence and have to find out which is the correct one

                    rule += "#" + word // mark that this word accours more than one time (for later processing)
                    if (k >= partOfTriple.split(" ").size - 1) {
                      rule += ";"
                    } else {
                      //println("ERROR1")
                      rule += " "
                    }
                  } else if (occurenceIndices.result().size == 1) {

                    rule += occurenceIndices.result()(0)
                    if (k >= partOfTriple.split(" ").size - 1) {
                      rule += ";"
                    } else {
                      //println("ERROR2")
                      rule += " "
                    }
                  } else {

                  }
                }
                }
              }
            }
          }
        }
        // take care of the words which occur more than one time
        if (!rule.isEmpty) {
          val ready = rule.dropRight(1).split(";").map(partOfTriple => {
            val partOfTripleSplitted = partOfTriple.trim.split(" ")
            partOfTripleSplitted.zipWithIndex.map { case (item, l) => {
              var wordBefore = ""
              var wordAfter = ""
              if (l - 1 >= 0) {
                if (partOfTripleSplitted(l - 1).head == '#') {
                  wordBefore = partOfTripleSplitted(l - 1).drop(1)
                } else {
                  wordBefore = listOfListsOfTuplesWithPOSAndWords(i)(partOfTripleSplitted(l - 1).toInt)._2
                }
              }
              if (l + 1 < partOfTripleSplitted.size) {
                if (partOfTripleSplitted(l + 1).head == '#') {
                  wordAfter = partOfTripleSplitted(l + 1).drop(1)
                } else {
                  wordAfter = listOfListsOfTuplesWithPOSAndWords(i)(partOfTripleSplitted(l + 1).toInt)._2
                }
              }

              if (item.head == '#') {
                val word = item.drop(1)
                val occurenceIndices = List.newBuilder[Int]
                listOfListsOfTuplesWithPOSAndWords(i).zipWithIndex.foreach { case (tuple, j) => if (tuple._2 == word) {
                  occurenceIndices += j
                }
                }
                var returnValue = ""
                val pointsForIndexList = List.newBuilder[Int]
                occurenceIndices.result().foreach(index => {
                  var pointsForIndex = 0
                  if ((index - 1) >= 0) {
                    val MaybeWordBeforeTuple = listOfListsOfTuplesWithPOSAndWords(i)(index - 1)
                    if (MaybeWordBeforeTuple._2 == wordBefore) {
                      pointsForIndex += 1
                    }
                  }
                  if ((index + 1) < listOfListsOfTuplesWithPOSAndWords(i).size) { // this is wrong
                    val MaybeWordAfterTuple = listOfListsOfTuplesWithPOSAndWords(i)(index + 1)
                    if (MaybeWordAfterTuple._2 == wordAfter) {
                      if (returnValue != "" && returnValue != index) {
                        //println("match not exact :( FIX ME!!  "+ listOfListsOfTuplesWithPOSAndWords(i)(index))
                      }
                      pointsForIndex += 1
                    }
                  }
                  pointsForIndexList += pointsForIndex
                })
                returnValue = "" + occurenceIndices.result()(pointsForIndexList.result().indexOf(pointsForIndexList.result().max))
                if (returnValue == "-1") {
                  returnValue = item
                }
                returnValue
              } else {
                item
              }
            }
            }
          })
          val readyAsString = ready.map(x => x.mkString(" ")).mkString(";")
          //println(readyAsString)
          rules += readyAsString
        }
      }
      if (rules.result().size > 1) {
        /*val extractions = rules.result().tail.mkString(",")
        client.execute {
          indexInto(("amazon_extractions_"+filename) / "doc") fields (
            "pattern" -> rules.result().head,
            "extractions" -> extractions
          )
        }*/
        rulesAll += rules.result()
      }

      /*rulesAll.result().foreach(x=> {
        val extractions = x.tail.mkString(",")
        client.execute {
          indexInto("amazon_extractions" / "doc") fields
            "pattern" -> x.head,
            "extractions" -> extractions
          )
        }*/
    }
    //rulesAll.clear()}

    //}
    //)

    println("getExtractions FINISHED")
    //client.close()
    return rulesAll.result()
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  def indexExtractions(filename: String, rulesAll: List[List[String]]): Unit = {

    println("indexExtractions READY")
    println("rulesAllSize: " + rulesAll.size)
    println("rulesAllSizeDistinct: " + rulesAll.distinct.size)
    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    client.execute {
      deleteIndex(("amazon_extractions_" + filename))
    }
    client.execute {
      createIndex(("amazon_extractions_" + filename)) mappings (
        mapping("doc") as(
          keywordField("pattern"),
          keywordField("extractions")
        ))
    }
    println("createIndex FINISHED")

    rulesAll.distinct.foreach(x => {
      val extractions = x.tail.mkString(",")
      client.execute {
        indexInto(("amazon_extractions_" + filename) / "doc") fields(
          "pattern" -> x.head,
          "extractions" -> extractions
        )
      }
    })
    client.close()
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  // MAIN
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  def main(args: Array[String]): Unit = {
    val numberOfProducts: Int = 10000

    var reviewName = ""
    def getCurrentFileName(): Unit = {
      (0 to 1000).foreach(index => {
        try {
          (Source.fromFile("data/review" + index + ".txt")); println("review" + index + " gibt es schon.")
        } catch {
          case e: FileNotFoundException => reviewName = "review" + index; return
        }
      }
      )
    }
    getCurrentFileName()

    println("Step 1 or 2?")
    val step: String = "2" //scala.io.StdIn.readLine()

    if (step == "1") {
      val listOfProducts = getListOfProducts("tools", numberOfProducts)
      println("N U M B E R  O F  P R O D U C T S: " + listOfProducts.size)
      val vectorOfReviews = saveAmazonReviewsInAFile(reviewName, listOfProducts)
      println("S I Z E - R E V I E W S: " + vectorOfReviews.size)
      val vectorOfSentences = ssplit(vectorOfReviews)
      println("S I Z E - S E N T E N C E S  A L L: " + vectorOfSentences.size)
      annotatePos(reviewName, vectorOfSentences)
    } else if (step == "2") {
      val rulesAll = parseExtractions("review67_ssplit_OIE5.txt")
      //indexExtractions(reviewName, rulesAll)
    }
    println("DONE")
  }
}
