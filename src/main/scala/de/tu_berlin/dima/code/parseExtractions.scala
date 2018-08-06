package de.tu_berlin.dima.code


import java.io.{File, PrintWriter}
import java.nio.charset.Charset

import com.google.common.io.Files
import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.http.HttpClient
import com.sksamuel.elastic4s.http.search.{SearchHit, SearchIterator}
import edu.stanford.nlp.ling.CoreAnnotations.{SentencesAnnotation, _}
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap
import opennlp.tools.chunker.{ChunkerME, ChunkerModel}

import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.concurrent.duration.Duration


class parseExtractions(client: HttpClient, pipelinePos: StanfordCoreNLP, model: ChunkerModel) {

  implicit val timeout = Duration(20, "seconds") // is the timeout for the SearchIterator.hits method
  case class chunkAndExtractions(chunks: String = "", extractionTriple: Vector[String] = Vector(""), count: Int = 0, precision: String = "", id: Int = 0, sentencesIndex: String = "")

  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  def chunking(sent: Array[String], pos: Array[String]): Array[String] = {
    val chunker: ChunkerME = new ChunkerME(model)
    val tag: Array[String] = chunker.chunk(sent, pos)
    //println(tag.zip(sent).foreach(println(_)))
    //println(tag.mkString(" "))
    tag
  }

  def getPOS(sentence: String): Vector[(String, String)] = { // get POS tags per sentence
    val document: Annotation = new Annotation(sentence)
    pipelinePos.annotate(document) // annotate
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

  def parseExtractionsController(filename: String, ElasticIndexName: String) = {

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // open extractions from file and split them
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    println("loadFiles")
    val inputFile1: File = new File("data/" + filename) // read from file
    val extractionBlocks: Vector[String] = Files.toString(inputFile1, Charset.forName("UTF-8")).split("\n\n").toVector

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // get sentences
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    val justSentences: Vector[String] = extractionBlocks.map(x => x.split("\n")(0))
    val justSentencesSize = justSentences.size
    println(s"We have ${justSentences.size} sentencens")

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // create index
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    createAnIndex(ElasticIndexName)

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // get sentences
    // - - - - - - - - - - - - - - - - - - - - - - - - -
    new PrintWriter(s"data/${filename}_sentences.txt") { // open new file
      justSentences.zip(extractionBlocks).zipWithIndex.foreach(x => {
        val parseExtractionsResult: Vector[chunkAndExtractions] = parseExtractions(filename, ElasticIndexName, Vector(x._1._1), Vector(x._1._2), x._2)
        //System.out.println(parseExtractionsResult)
        indexExtractions(ElasticIndexName, parseExtractionsResult)
        write(x._2 + " " + x._1._1 + "\n") // write distinct list to file
        if (x._2 % 100 == 0) System.out.println(x._2+" of "+justSentencesSize)
      })
      close // close file
    }
  }


  // - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  def parseExtractions(filename: String, ElasticIndexName: String, sentence: Vector[String], extractionBlock: Vector[String], sentenceID: Int): Vector[chunkAndExtractions] = {


    val justChunks: Seq[Array[String]] = sentence.map(sent => {
      val pos = getPOS(sent)
      val chunked: Array[String] = chunking(pos.map(x => x._2).toArray, pos.map(x => x._1).toArray)
      val chunkedReplacedO = chunked.zipWithIndex.map { case (chunk, i) => {
        if (chunk == "O") {
          pos(i)._1
        } else {
          chunk
        }
      }
      }
      chunkedReplacedO
    })

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // get a Vector with tokenized sentence and their extractions tokenized as well
    // some filtering
    // result = extractions
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    val extractionsPrecisions: Vector[List[String]] = extractionBlock
      .map(_.split("\n").toList) // split the lines in one extraction part
      .map(extrLines => {
      extrLines.map(line => {
        line.split(" ").take(1).mkString("")
      })
    })

    val extractions: Vector[List[List[String]]] = extractionBlock
      .map(_.split("\n")) // split the lines in one extraction part
      .map(_.toList)
      .map(extractionPart => extractionPart
        .map(line => {

          val indexList = line.indexOf("List([")

          def getlineCleaned(line: String): String = {
            if (indexList >= 0) {
              val indexColon = line.indexOf(":")
              line.drop(indexColon + 1)
            } else {
              line
            }
          }

          val lineCleaned = getlineCleaned(line)


          val indexFirst = lineCleaned.indexOf("(")
          val indexLast = lineCleaned.lastIndexOf(")")
          if (indexFirst == -1 || indexLast == -1) {
            getPOS(lineCleaned).map(_._2).toList // when the line is the original sentence tokenize it
          } else {
            lineCleaned.drop(indexFirst + 1).dropRight(lineCleaned.size - indexLast).split(";").toList.map(x => x.trim)
              .filter(x => x != "") // filter triple  which are actually tuples //TODO Geht das wirklich gut?
          }
        })
        .filter(line => !line.mkString(" ").contains("List([")) // filter all Context Extractions of OIE5 since they do not deliver usefull extractions anyways
        //.filter(line => !line.mkString(" ").contains("T:"))// filter all time Extractions of OIE5
        //.filter(line => !line.mkString(" ").contains("L:"))// filter all location Extractions of OIE5
      )

    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // We want to know which word in the extraction matches with which word in the sentence
    // result: rulesAll
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    var counter3 = 0
    val rulesAll = Vector.newBuilder[Vector[String]]
    val precisionsAll = Vector.newBuilder[Vector[String]]
    val sentencesAll = Vector.newBuilder[Int]
    // e.g. rulesAll.result():
    // Vector(Vector(, 1;2 3 4;6 8;)) // the inner Vector stands for the different rules for ONE sentence
    // for the sentence:
    // So this is really for accessories and big parts
    // 0,38 (this; is really for; and parts)
    val parsedExtractionsSize = extractions.size
    for (extractionIndex <- extractions.indices) {
      //println("rulesAll: " + extractionIndex + " of " + parsedExtractionsSize)
      val rules = Vector.newBuilder[String]
      val precisions = Vector.newBuilder[String]

      for (lineNumberOfExtraction <- extractions(extractionIndex).indices) {
        var rule: String = ""

        if (lineNumberOfExtraction != 0) { // exclude the sentence itself (only extraction lines are considered)
          if (!(extractions(extractionIndex)(lineNumberOfExtraction)(0) == "No extractions found.")) { // ollie uses this line to indicate that there is no extracion
            if (extractions(extractionIndex)(lineNumberOfExtraction).size >= 2) {

              for (partOfTriple <- extractions(extractionIndex)(lineNumberOfExtraction)) {

                val partOfTripleCleaned = partOfTriple.split(" ").map(token => {
                  if (token.contains("T:") || token.contains("L:")) {
                    token.drop(2)
                  } else token
                }).mkString(" ")

                val partOfTripleToken: Vector[String] = getPOS(partOfTripleCleaned).map(_._2)

                partOfTripleToken.zipWithIndex.foreach { case (wordInExtracion, wordInExtracionIndex) => { // for every word in one triple part

                  var occurenceIndices: String = "wordNotInSentence"
                  var wordBeforeInSentence = ""
                  var wordAfterInSentence = ""
                  var wordBeforeInTriple = partOfTripleToken.lift(wordInExtracionIndex - 1) match {
                    case Some(x) => x;
                    case None => ""
                  }
                  var wordAfterInTriple = partOfTripleToken.lift(wordInExtracionIndex + 1) match {
                    case Some(x) => x;
                    case None => ""
                  }

                  extractions(extractionIndex)(0).zipWithIndex.foreach { case (wordInSentence, j) => if (wordInSentence == wordInExtracion) {
                    wordBeforeInSentence = extractions(extractionIndex)(0).lift(j - 1) match {
                      case Some(x) => x;
                      case None => ""
                    }
                    wordAfterInSentence = extractions(extractionIndex)(0).lift(j + 1) match {
                      case Some(x) => x;
                      case None => ""
                    }

                    if (occurenceIndices == "wordNotInSentence") {
                      if (wordBeforeInSentence == wordBeforeInTriple) {
                        occurenceIndices = j.toString

                      } else if (wordAfterInSentence == wordAfterInTriple) {
                        occurenceIndices = j.toString

                      } else {
                        occurenceIndices = j.toString
                      }
                    } else {
                      if (wordBeforeInSentence == wordBeforeInTriple) {
                        occurenceIndices = j.toString

                      } else if (wordAfterInSentence == wordAfterInTriple) {
                        occurenceIndices = j.toString

                      }
                    }

                  }
                  }


                  rule += occurenceIndices


                  if (wordInExtracionIndex >= partOfTriple.split(" ").size - 1) {
                    rule += ";"
                  } else {
                    rule += " "
                  }

                }
                }
              }

            }
          }

        }

        rules += rule
        precisions += extractionsPrecisions(extractionIndex)(lineNumberOfExtraction)

      }
      val rulesAndPrecisions: Vector[(String, String)] = rules.result().zip(precisions.result()).filterNot(_._1.isEmpty)
      rulesAll += rulesAndPrecisions.map(_._1)
      precisionsAll += rulesAndPrecisions.map(_._2)
      sentencesAll += extractionIndex
    }
    val rules: Vector[Vector[String]] = rulesAll.result()
    val precisions: Vector[Vector[String]] = precisionsAll.result()
    val sentences: Vector[Int] = sentencesAll.result()


    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // We transform rulesAll: now, the extraction matches the CTS (chunk tag sequence)
    // delete the extraction where wordNotInSentence
    // LOGIC:
    // We take a Chunk pattern , say B-NP I-NP I-NP B-PP B-NP, and a extraction rule, say 0,1,2;3;4, and delete the I-* as from the beginning to the end of both lists
    // after one round it would look like: B-NP I-NP B-PP B-NP / 0,1;2;3 -> 1 has been deleted in the second list and the rest following count one down
    // after two rounds it would look like: B-NP B-PP B-NP / 0;1;2 -> 1 has been deleted in the second list and the rest following count one down
    // FINISH
    // result: chunksAndExtractions
    // - - - - - - - - - - - - - - - - - - - - - - - - -

    val chunksAndExtractions = Vector.newBuilder[chunkAndExtractions]
    // e.g. "So this is really for accessories and big parts"
    // chunksAndExtractions: Vector(chunkAndExtractions(O$B-NP$B-VP$B-ADVP$B-PP$B-NP$O$B-NP,Vector(1, 2 3 4, 6 7),0))
    var misscreatedExtraction = 0
    var wordNotInSentenceCounter = 0
    var counterId = 0
    for (i <- 0 to rules.size - 1) {
      val rulesOfSentence: Vector[String] = rules(i)
      val precisionsOfSentence: Vector[String] = precisions(i)
      val chunksOfSentence: Vector[String] = justChunks(i).toVector // (B-NP, B-PP, B-NP)

      // this part a vector which contain a reduces token Indice
      // result like (0 1, 2, 3)

      val chunkedTagsReduced = Vector.newBuilder[String] // hier wird der reduzierte chunk Vector gesteichert
      chunkedTagsReduced += chunksOfSentence(0) // fügt den ersten chunk hinzu (eg. B-NP)
      val tokenIndices: Seq[String] = chunksOfSentence.indices.map(_.toString) // erstellt Liste mit den indices zu den chunks (0,1,2)
      val tokenIndicesReduced = Vector.newBuilder[String] // hier wird der reduzierte Indice Vector gespeichert
      var tokenIndicesReducedTMP: String = tokenIndices(0) // nimmt sich den ersten index
      for (i <- 1 until chunksOfSentence.size) {
        if (chunksOfSentence(i)(0).toString != "I") {
          chunkedTagsReduced += chunksOfSentence(i)
          tokenIndicesReduced += tokenIndicesReducedTMP
          tokenIndicesReducedTMP = ""
          tokenIndicesReducedTMP += " " + tokenIndices(i)
        } else {
          tokenIndicesReducedTMP += " " + tokenIndices(i)
        }
      }
      tokenIndicesReduced += tokenIndicesReducedTMP

      val tokenIndicesReducedResult = tokenIndicesReduced.result() // (0 1,  2,  3,  4,  5 6 7 8 9)

      val chunkedTagsReducedResult = chunkedTagsReduced.result() // (B-NP, B-VP, B-NP, B-PP, B-NP, .)


      for (j <- 0 to rulesOfSentence.size - 1) { // für jede extractions Regel

        val ruleOfSentence: String = rulesOfSentence(j)
        val precisionOfSentence: String = precisionsOfSentence(j)
        val ruleOfSentenceTriple: Vector[String] = ruleOfSentence.split(";").toVector
          .filter(_ != "") // filter parts of triple which are empty (usually the part at the and 0;1 3 4 5 6 7 8 9;2;)
        val containswordNotInSentence: Boolean = ruleOfSentence.contains("wordNotInSentence")
        if (ruleOfSentenceTriple.size >= 2 && !containswordNotInSentence) { // the triple must have at least 2 parts otherwise its not a valid extraction


          var alreadyUsedIndices = Vector.newBuilder[Int] // here we store indices which are already present in the pattern
          val ruleOfSentenceTripleNew = ruleOfSentenceTriple.map(triplePart => {
            val triplePartSplitted: Array[String] = triplePart.split(" ").filter(_ != "")
            var poristionIntokenIndicesReducedResult: Vector[Int] = (for (index <- triplePartSplitted) yield {
              val indexOfChunk = tokenIndicesReducedResult.map(chunkWithIndices => chunkWithIndices.contains(index)).indexOf(true)
              if (alreadyUsedIndices.result().contains(indexOfChunk)) {
                None
              } else {
                alreadyUsedIndices += indexOfChunk
                Some(indexOfChunk)
              }
            }).toVector.flatten
            poristionIntokenIndicesReducedResult.mkString(" ")
          })


          counterId += 1
          chunksAndExtractions += chunkAndExtractions(chunkedTagsReducedResult.mkString("$"), ruleOfSentenceTripleNew, precision = precisionOfSentence, sentencesIndex = sentence(sentences(i)), id = sentenceID)

          //OLD
          /*
          var chunksOfSentenceUpdated: Vector[String] = chunksOfSentence

          var ruleOfSentenceTripleUpdated: Vector[String] = ruleOfSentenceTriple

          while (chunksOfSentenceUpdated.map(x => x(0).toString).contains("I")) {
            val chunksOfSentenceOnlyFirstChar: Vector[String] = chunksOfSentenceUpdated.map(x => x(0).toString)
            val indexOfI = chunksOfSentenceOnlyFirstChar.indexOf("I")
            chunksOfSentenceUpdated = chunksOfSentenceUpdated.take(indexOfI) ++ chunksOfSentenceUpdated.drop(indexOfI + 1)
            ruleOfSentenceTripleUpdated = ruleOfSentenceTripleUpdated.map(part => {
              val partSplitted = part.split(" ").filter(_ != "")
              var ruleOfSentenceTripleUpdatedPartAsVector: Vector[Int] = Vector[Int]()
              val firstIndice = partSplitted.head

              if (part.split(" ")(0) != "") {
                ruleOfSentenceTripleUpdatedPartAsVector = part.split(" ").map(_.toInt).toVector
              } else {
                // That happens in case another chunk eats the chunk which is located in this chunk e.g.:
                // I was constantly losing my debit card and drivers license
                // 0,22 (I; was losing my debit card and drivers license; T:constantly)
                // constantly belongs to the chunk of "was constantly losing" (VP)
              }

              // count the remaining indices down
              val ruleOfSentenceTripleUpdatedPartAsVectorUpdated: Vector[Int] = ruleOfSentenceTripleUpdatedPartAsVector.filter(x => {
                if (x == indexOfI) false else true
              }).map(x => {
                if (x >= indexOfI) x - 1 else x
              })

              // this part a vector which contain a reduces token Indice
              // result like (0 1, 2, 3)
              val replacementForEmtyPart: Option[Vector[String]] = if(ruleOfSentenceTripleUpdatedPartAsVectorUpdated.isEmpty){
                val replacementForEmtyPart: Vector[Option[Vector[String]]] = for(partIndices <- tokenIndicesReducedResult) yield {
                  val partIndicesAsVector = partIndices.trim.split("").toVector
                  val firstIndiceInPart = partIndicesAsVector.indexOf(firstIndice)
                  if (firstIndiceInPart >= 0 && partIndicesAsVector(0) != firstIndice.toString) {
                    Some(Vector(partIndicesAsVector(0)))
                  } else {
                    None
                  }
                }
                replacementForEmtyPart.flatten.headOption

              }else{None}


              println(replacementForEmtyPart)
              println(ruleOfSentenceTripleUpdatedPartAsVectorUpdated)

              replacementForEmtyPart match {
                case Some(newIndice) => newIndice.mkString("")
                case None => ruleOfSentenceTripleUpdatedPartAsVectorUpdated.mkString(" ")
              }
              ruleOfSentenceTripleUpdatedPartAsVectorUpdated.mkString(" ")
            })
          }

          if (!ruleOfSentenceTripleUpdated.contains("")) {


            /*if(chunksOfSentenceUpdated.mkString("$") == "B-NP$B-VP$B-NP$B-PP$B-NP"){
              //println(justChunks(i).mkString(" "))
              println(sentence(i))
            }*/

            chunksAndExtractions += chunkAndExtractions(chunksOfSentenceUpdated.mkString("$"), ruleOfSentenceTripleUpdated,precision = precisionOfSentence)
          } else {
            println("")
            println("ERROR: misscreatedExtraction")
            println("sentence(i): " + sentence(i))
            println("extractions(i): " + extractions(i)(j + 1))
            println("ruleOfSentence: " + ruleOfSentence)
            println("ruleOfSentenceTripleUpdated: " + ruleOfSentenceTripleUpdated)
            println("chunksOfSentence: " + chunksOfSentence.zip(getPOS(sentence(i)).map(_._2).toList))
            println("POS: " + getPOS(sentence(i)))
            println("")
            misscreatedExtraction += 1
          }
          */
        } else {
          wordNotInSentenceCounter += 1
          //println("wordNotInSentenceCounter: " + wordNotInSentenceCounter)
          /*println("")
          println("ERROR: wordNotInSentence")
          println("sentence(i): " + sentence(i))
          println("extractions(i): " + extractions(i)(j + 1))
          println("ruleOfSentence: " + ruleOfSentence)
          println("chunksOfSentence: " + chunksOfSentence.zip(getPOS(sentence(i)).map(_._2).toList))
          println("")*/
        }
      }
    }

    // misscreatedExtraction:
    // That happens in case another chunk eats the chunk which is located in this chunk e.g.:
    // I was constantly losing my debit card and drivers license
    // 0,22 (I; was losing my debit card and drivers license; T:constantly)
    // constantly belongs to the chunk of "was constantly losing" (VP)


    // - - - - - - - - - - - - - - - - - - - - - - - - -
    // take care of duplicated extraction rules
    // - - - - - - - - - - - - - - - - - - - - - - - - -


    val chunksAndExtractionsResult: Vector[chunkAndExtractions] = chunksAndExtractions.result()
    /*var chunksAndExtractionsDisjunct = Vector[chunkAndExtractions]()
    var counter5 = 0
    for (i <- 0 until chunksAndExtractionsResult.size) {
      if(counter5 % 1000 == 0) println("counter5: " + counter5+ " : "+chunksAndExtractionsResult.size)
      counter5 += 1
      val chunksAndExtraction = chunkAndExtractions(chunksAndExtractionsResult(i).chunks, chunksAndExtractionsResult(i).extractionTriple, 1, chunksAndExtractionsResult(i).precision,i,sentence(chunksAndExtractionsResult(i).sentencesIndex.toInt))

      var chunkFoundIndex: Int = -1
      var chunkFoundExtraction = None: Option[chunkAndExtractions]
      for (j <- 0 until chunksAndExtractionsDisjunct.size) {
        val chunksAndExtractionDisjunkt = chunksAndExtractionsDisjunct(j)
        if (chunksAndExtraction.chunks == chunksAndExtractionDisjunkt.chunks && chunksAndExtraction.extractionTriple.mkString("") == chunksAndExtractionDisjunkt.extractionTriple.mkString("")) {
          chunkFoundIndex = j
          chunkFoundExtraction = Some(chunksAndExtractionDisjunkt)
        }
      }
      chunkFoundExtraction match {
        case None => chunksAndExtractionsDisjunct = chunksAndExtractionsDisjunct ++ Vector(chunksAndExtraction)
        case Some(x) => {
          val chunk = x.chunks
          val extractionTriple = x.extractionTriple
          val id = x.id
          val newCount: Int = chunksAndExtractionsDisjunct(chunkFoundIndex).count + 1
          val newPricsion: String = chunksAndExtractionsDisjunct(chunkFoundIndex).precision + " "+chunksAndExtraction.precision
          val newSentences = x.sentencesIndex + " SPLITHERE "+chunksAndExtraction.sentencesIndex
          chunksAndExtractionsDisjunct = chunksAndExtractionsDisjunct.updated(chunkFoundIndex, chunkAndExtractions(chunk, extractionTriple, newCount,newPricsion,id,newSentences))
        }
      }
    }*/
    chunksAndExtractionsResult

  }


  // - - - - - - - - - - - - - - - - - - - - - - - - -
  // createIndex
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  def createAnIndex(ElasticIndexName: String): Unit = {

    println("")
    println("establish connection to ES")
    client.execute {
      deleteIndex((ElasticIndexName))
    }
    client.execute {
      deleteIndex((ElasticIndexName + "_sentences"))
    }

    println("create Index")
    client.execute {
      createIndex((ElasticIndexName)) mappings (
        mapping("doc") as(
          keywordField("chunkPattern"),
          keywordField("precision"),
          keywordField("extraction"),
          intField("id")
        ))
    }
  }


  // - - - - - - - - - - - - - - - - - - - - - - - - -
  // indexExtractions
  // - - - - - - - - - - - - - - - - - - - - - - - - -

  def indexExtractions(ElasticIndexName: String, chunksAndExtractions: Vector[chunkAndExtractions]): Unit = {

    chunksAndExtractions.foreach(x => {
      //for (i <- 0 to chunksAndExtractions.size - 1) {
      val chunkAndExtraction = x
      client.execute {
        indexInto((ElasticIndexName) / "doc").fields(
          "pattern" -> chunkAndExtraction.chunks,
          "extractions" -> chunkAndExtraction.extractionTriple.mkString(","),
          "precision" -> chunkAndExtraction.precision,
          "id" -> chunkAndExtraction.id
        )
      }.await
    })
  }
}
