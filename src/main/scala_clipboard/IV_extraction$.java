package de.tu_berlin.dima.code;

object IV_extraction {

  // METHODS

  def getExtractions(filename: String):List[List[String]] = {
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
    val inputFile1: File = new File(filename + "ssplit_extr.txt") // read from file
    //val inputFile2: File = new File(filename + "ssplit.txt") // read from file
    val inputFile3: File = new File(filename + "ssplit_POS.txt") // read from file
    var extr: Vector[String] = Files.toString(inputFile1, Charset.forName("UTF-8")).split("\n\n").toVector
    //var text2: List[String] = Files.toString(inputFile2, Charset.forName("UTF-8")).split("\n").toVector
    var pos: Vector[String] = Files.toString(inputFile3, Charset.forName("UTF-8")).split("\n").toVector
    val (text1,text3) = (extr,pos).zipped.toVector.filter(x=>x._2.length<=10).unzip


    // this value lookes like: List((PRP,I), (VBP,like), (PRP,it), (.,.))
    println("listOfListsOfTuplesWithPOSAndWords")
    val listOfListsOfTuplesWithPOSAndWords: Vector[List[(String, String)]] = text3.map(x => x.split("\\s").toList.map(x => {
      val y = x.drop(1).dropRight(1)

      val z = y.split("\\,")
      if (z.size == 2) {
        (z(0), z(1))
      }else if(y.equals(",,,")){
        (",", ",")
      } else {
        //println("unknown POS: "+y)
        ("", "")
      }
    })).toVector.map(x=>x.filter(x=>{if(x._1 == "," && x._1 == ","){false}else{true}})) // filter all (",",",")

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

      rules += (for(i<-listOfListsOfTuplesWithPOSAndWords(i))yield{i._1}).mkString(" ")
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
  //////////
  // Index extractions
  //////////
  def indexExtractions(filename:String, rulesAll: List[List[String]]): Unit = {

    println("indexExtractions READY")
    println("rulesAllSize: "+rulesAll.size)
    println("rulesAllSizeDistinct: "+rulesAll.distinct.size)
    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    client.execute { deleteIndex(("amazon_extractions_"+filename))}
    client.execute { createIndex(("amazon_extractions_"+filename)) mappings (
        mapping("doc") as (
          keywordField("pattern"),
          keywordField("extractions")
        ))
    }
    println("createIndex FINISHED")

    rulesAll.distinct.foreach(x=> {
      val extractions = x.tail.mkString(",")
      client.execute {
        indexInto(("amazon_extractions_"+filename) / "doc") fields (
          "pattern" -> x.head,
          "extractions" -> extractions
        )
      }
    })
    client.close()
  }

  // MAIN
  def main(args: Array[String]): Unit = {
    val filename = "reviews24740"
    //val filename = "reviews10000"

    val rulesAll = getExtractions(filename)
    indexExtractions(filename, rulesAll)

  }
}
