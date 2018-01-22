package de.tu_berlin.dima.code

import java.util.Properties

import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.http.HttpClient
import edu.stanford.nlp.ling.CoreAnnotations.{PartOfSpeechAnnotation, SentencesAnnotation, TokensAnnotation}
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.concurrent.duration.Duration

object V_testExtract {

  // METHODS

  ////////////////////
  // get a List of all with Product id's of a given category
  ////////////////////

  def getListOfProducts(category: String, numberOfProducts:Int): List[String] = {
    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    implicit val timeout = Duration(20, "seconds") // is the timeout for the SearchIterator.hits method
    val listBuilder = List.newBuilder[String]

    val resp = client.execute {
      search("amazon_reviews_metadata" / "doc").keepAlive("1m").size(numberOfProducts) query fuzzyQuery("categories", category).fuzziness("1")
    }.await()
    resp match {
      case Left(failure) => println("We failed " + failure.error)
      case Right(results) => results.result.hits.hits.foreach(x => {listBuilder += x.sourceField("asin").toString})
    }

    //println(listBuilder.result().size)
    listBuilder.result()
  }

  ////////////////////
  // get a string with all reviews
  ////////////////////

  def getAmazonReviewsAsList(numberOfReviews: Int, listOfProducts:List[String]):String ={


    val listOfListsOfProducts = listOfProducts.grouped(100).toList

    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    val listBuilder = List.newBuilder[String]

    //var counterForIExistToLimitSomething = 0
    //def iExistToLimitSomething: Unit = {
      for (i <- listOfListsOfProducts.indices) {
        //counterForIExistToLimitSomething += 1
        //println(i)
        val resp = client.execute {
          println("This is the list of products: "+listOfListsOfProducts(i))
          search("amazon_reviews_all" / "doc").keepAlive("1m").size(10000) query termsQuery("asin.keyword", listOfListsOfProducts(i))
        }.await
        resp match {
          case Left(failure) => println("We failed " + failure.error)
          case Right(results) => results.result.hits.hits.foreach(x => {
            listBuilder += x.sourceField("reviewText").toString
          })
        }
      }
    //}


    println("This is the number of reviews: " + listBuilder.result().size)
    client.close() // close HttpClient
    listBuilder.result().mkString("\n")
  }

  ////////////////////
  // get a list with all ssplit reviews
  ////////////////////

  def ssplit(text:String):List[String] = {

    val props: Properties = new Properties()
    props.put("annotators", "tokenize, ssplit")

    val pipeline: StanfordCoreNLP = new StanfordCoreNLP(props)

    // create blank annotator
    val document: Annotation = new Annotation(text)
    // run all Annotator - Tokenizer on this text
    pipeline.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).asScala.toList

    (for {
      sentence: CoreMap <- sentences
    } yield (sentence)).map(_.toString)
  }

  ////////////
  // get a list of annotated sentences
  ////////////

  def annotatePos(filename:Vector[String]):Vector[List[(String, String)]] = {
    val props: Properties = new Properties() // set properties for annotator
    props.put("annotators", "tokenize, ssplit,pos") // set properties

    val pipeline: StanfordCoreNLP = new StanfordCoreNLP(props) // annotate file

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
    filename
      .map(getPOS(_)) // get POS tags
    //.map(_.mkString(" ")) // make List of POS tags to String
  }

  /////////
  // test extractions
  /////////
  def testExtraction(filename:String): Unit = {

    val props: Properties = new Properties() // set properties for annotator
    props.put("annotators", "tokenize, ssplit,pos") // set properties
    val pipeline: StanfordCoreNLP = new StanfordCoreNLP(props) // annotate file

    val numberOfProducts:Int = 100
    val numberOfReviews:Int = 100 // no effects right now
    println("listOfProductsAsins")
    val listOfProductsAsins = getListOfProducts("Watches",numberOfProducts)
    println("listOfProductsReviews")
    val listOfProductsReviews: String = getAmazonReviewsAsList(numberOfReviews,listOfProductsAsins)
    println("listOfProductsReviewsSsplited")
    val listOfProductsReviewsSsplited: Vector[String] = ssplit(listOfProductsReviews).toVector
    println("listOfProductsReviewsPOSTagged")
    val listOfProductsReviewsPOSTagged: Vector[List[(String, String)]] = annotatePos(listOfProductsReviewsSsplited).toVector
    println("listOfProductsReviewsPOSTaggedToken")
    val listOfProductsReviewsPOSTaggedToken: Vector[List[String]] = listOfProductsReviewsPOSTagged.map(x => {for(i <-x)yield {i._2}}).toVector
    println("listOfProductsReviewsPOSTaggedPOS")
    val listOfProductsReviewsPOSTaggedPOS: Vector[List[String]] = listOfProductsReviewsPOSTagged.map(x => {for(i <-x)yield {i._1}}).toVector
    println("EXTRACT :)")

    val matchNoMatch: immutable.Seq[Int] = for(sentenceNumber <- listOfProductsReviewsSsplited.indices)yield{
      println(sentenceNumber)
      val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
      // {"version":true,"query":{"term":{"pattern":{"value":"NN VBD RB IN JJ CC JJ IN VBD VBN ."}}}}
      val resp = client.execute {
        search(("amazon_extractions_"+filename) / "doc") query termQuery("pattern", listOfProductsReviewsPOSTaggedPOS(sentenceNumber).mkString(" "))
      }.await
      var matched = ""
      resp match {
        case Left(failure) => println("We failed " + failure.error)
        case Right(results) => results.result.hits.hits.foreach(x => {matched += x.sourceField("extractions").toString;matched+=","})
      }
      client.close()
      if(!matched.isEmpty){

        val rules: Array[Array[Array[String]]] = matched.split(",").distinct.map(x => x.split(";").map(x=>x.split(" ")))

        var extractions = ""
        rules.foreach(x=>{x.foreach(partOfTriple=>{partOfTriple.foreach(index=>{extractions += listOfProductsReviewsPOSTaggedToken(sentenceNumber)(index.toInt)+" "});extractions=extractions.trim;extractions+=";"});extractions=extractions.dropRight(1);extractions +=","})

        /*println("\n/\n")
        println("Sentence: "+listOfProductsReviewsSsplited(sentenceNumber))
        println("Extractions: "+extractions.dropRight(1))*/
        1
      }else{
        0
      }
    }

    println("matches: "+matchNoMatch.filter(x => x==1).size)
    println("no matches: "+matchNoMatch.filter(x => x==0).size)

    /*
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

    val sentence = "Color was not as bright and deep as was displayed."
    val sentenceParsed = getPOS(sentence)
    val sentenceTokenized: Seq[String] = (for(tuple<-sentenceParsed)yield{tuple._2})
    val sentencePOS: String = (for(tuple<-sentenceParsed)yield{tuple._1}).mkString(" ")


    println(sentence)

    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client

    // {"version":true,"query":{"term":{"pattern":{"value":"NN VBD RB IN JJ CC JJ IN VBD VBN ."}}}}
    val resp = client.execute {
      search("amazon_extractions" / "doc") query termQuery("pattern", sentencePOS)
    }.await
    var matched = ""
    resp match {
      case Left(failure) => println("We failed " + failure.error)
      case Right(results) => results.result.hits.hits.foreach(x => {matched += x.sourceField("extractions").toString})
    }
    client.close()
    val rules = matched.split(",").map(x => x.split(";").map(x=>x.split(" ")))
    //val extractions = List.newBuilder[String]

    var extractions = ""
    rules.foreach(x=>x.foreach(partOfTriple=>{partOfTriple.foreach(index=>{extractions += sentenceTokenized(index.toInt)+" "});extractions=extractions.trim;extractions+=";"}))
    println(extractions.dropRight(1))
    */
  }


  // MAIN
  def main(args: Array[String]): Unit = {
    testExtraction("reviews10000")
  }
}
