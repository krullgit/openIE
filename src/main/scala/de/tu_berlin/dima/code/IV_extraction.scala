package de.tu_berlin.dima.code

import java.io.File
import java.nio.charset.Charset

import com.google.common.io.Files
import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.http.HttpClient

object IV_extraction {

  // METHODS

  def getExtractions(filename: String):List[List[String]] = {

    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    client.execute { deleteIndex("amazon_extractions_"+filename)}
    client.execute { createIndex("amazon_extractions_"+filename) mappings (
      mapping("doc") as (
        keywordField("pattern"),
        keywordField("extractions")
      ))
    }

    println("loadFiles")
    val inputFile1: File = new File(filename + "ssplit_extr.txt") // read from file
    val inputFile2: File = new File(filename + "ssplit.txt") // read from file
    val inputFile3: File = new File(filename + "ssplit_POS.txt") // read from file
    val text1: List[String] = Files.toString(inputFile1, Charset.forName("UTF-8")).split("\n\n").toList
    val text2: List[String] = Files.toString(inputFile2, Charset.forName("UTF-8")).split("\n").toList
    val text3: List[String] = Files.toString(inputFile3, Charset.forName("UTF-8")).split("\n").toList


    // this value lookes like: List((PRP,I), (VBP,like), (PRP,it), (.,.))
    println("listOfListsOfTuplesWithWordAndPOS")
    val listOfListsOfTuplesWithWordAndPOS: Vector[List[(String, String)]] = text3.map(x => x.split("\\s").toList.map(x => {
      val y = x.drop(1).dropRight(1).split("\\,")
      if (y.size == 2) {
        (y(0), y(1))
      } else {
        ("", "")
      }
    })).toVector

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
      println("rulesAll: "+i+" of "+parsedExtractionsSize)
      val rules = List.newBuilder[String]

      rules += (for(i<-listOfListsOfTuplesWithWordAndPOS(i))yield{i._1}).mkString(" ")
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

                  listOfListsOfTuplesWithWordAndPOS(i).zipWithIndex.foreach { case (tuple, j) => if (tuple._2 == word) {
                    occurenceIndices += j
                  }
                  }
                  if (occurenceIndices.result().size > 1) { // we found more than one occurence of the current word in the sentence and have to find out which is the correct one

                    rule += "#" + word // mark that this word accours more than one time (for later processing)
                    if (k >= partOfTriple.split(" ").size - 1) {
                      rule += ";"
                    } else {
                      rule += " "
                    }
                  } else if (occurenceIndices.result().size == 1) {

                    rule += occurenceIndices.result()(0)
                    if (k >= partOfTriple.split(" ").size - 1) {
                      rule += ";"
                    } else {
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
                  wordBefore = listOfListsOfTuplesWithWordAndPOS(i)(partOfTripleSplitted(l - 1).toInt)._2
                }
              }
              if (l + 1 < partOfTripleSplitted.size) {
                if (partOfTripleSplitted(l + 1).head == '#') {
                  wordAfter = partOfTripleSplitted(l + 1).drop(1)
                } else {
                  wordAfter = listOfListsOfTuplesWithWordAndPOS(i)(partOfTripleSplitted(l + 1).toInt)._2
                }
              }

              if (item.head == '#') {
                val word = item.drop(1)
                val occurenceIndices = List.newBuilder[Int]
                listOfListsOfTuplesWithWordAndPOS(i).zipWithIndex.foreach { case (tuple, j) => if (tuple._2 == word) {
                  occurenceIndices += j
                }
                }
                var returnValue = ""
                val pointsForIndexList = List.newBuilder[Int]
                occurenceIndices.result().foreach(index => {
                  var pointsForIndex = 0
                  if ((index - 1) >= 0) {
                    val MaybeWordBeforeTuple = listOfListsOfTuplesWithWordAndPOS(i)(index - 1)
                    if (MaybeWordBeforeTuple._2 == wordBefore) {
                      pointsForIndex += 1
                    }
                  }
                  if ((index + 1) < listOfListsOfTuplesWithWordAndPOS(i).size) { // this is wrong
                    val MaybeWordAfterTuple = listOfListsOfTuplesWithWordAndPOS(i)(index + 1)
                    if (MaybeWordAfterTuple._2 == wordAfter) {
                      if (returnValue != "" && returnValue != index) {
                        //println("match not exact :( FIX ME!!  "+ listOfListsOfTuplesWithWordAndPOS(i)(index))
                      }
                      pointsForIndex += 1
                    }
                  }
                  pointsForIndexList += pointsForIndex
                })
                returnValue = "" + occurenceIndices.result()(pointsForIndexList.result().indexOf(pointsForIndexList.result().max))
                if (returnValue == "-1" ) {
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
      if(rules.result().size>1){
        val extractions = rules.result().tail.mkString(",")
        client.execute {
          indexInto(("amazon_extractions_"+filename) / "doc") fields (
            "pattern" -> rules.result().head,
            "extractions" -> extractions
          )
        }
        //rulesAll += rules.result()
      }
      /*
      rulesAll.result().foreach(x=> {
        val extractions = x.tail.mkString(",")
        client.execute {
          indexInto("amazon_extractions" / "doc") fields (
            "pattern" -> x.head,
            "extractions" -> extractions
          )
        }
      })*/
      rulesAll.clear()
    }
    //}
    //)

    println("getExtractions FINISHED")
    client.close()
    return rulesAll.result()
  }
  /////////
  // Index extractions
  /////////
  def indexExtractions(rulesAll: List[List[String]]): Unit = {
    println("indexExtractions READY")
    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    client.execute { deleteIndex("amazon_extractions")}
    client.execute { createIndex("amazon_extractions") mappings (
        mapping("doc") as (
          keywordField("pattern"),
          keywordField("extractions")
        ))
    }
    println("createIndex FINISHED")

    rulesAll.foreach(x=> {
      val extractions = x.tail.mkString(",")
      client.execute {
        indexInto("amazon_extractions" / "doc") fields (
          "pattern" -> x.head,
          "extractions" -> extractions
        )
      }
    })
    client.close()
  }

  // MAIN
  def main(args: Array[String]): Unit = {
    //val rulesAll = getExtractions("reviews10000")
    val rulesAll = getExtractions("reviews24740")
    //indexExtractions(rulesAll)

  }
}
