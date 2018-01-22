package de.tu_berlin.dima.code

import java.io.PrintWriter

import com.sksamuel.elastic4s.ElasticsearchClientUri
import com.sksamuel.elastic4s.http.ElasticDsl._
import com.sksamuel.elastic4s.http.HttpClient

import scala.concurrent.duration.Duration

object saveAmazonReviewsInAFile {

  // METHODS

  ////////////////////
  // get a file with all reviews
  ////////////////////

  def saveAmazonReviewsInAFile(numberOfReviews: Int, listOfProducts:List[String]) {


    val listOfListsOfProducts = listOfProducts.grouped(100).toList

    val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    val listBuilder = List.newBuilder[String]
    for (i <- listOfListsOfProducts.indices ) {
      println(i)
      val resp = client.execute {
        search("amazon_reviews_all" / "doc").keepAlive("1m").size(10000) query termsQuery("asin.keyword", listOfListsOfProducts(i))
      }.await
      resp match {
        case Left(failure) => println("We failed " + failure.error)
        case Right(results) => results.result.hits.hits.foreach(x => {listBuilder += x.sourceField("reviewText").toString})
      }
    }


    println(listBuilder.result().size)
    client.close() // close HttpClient

    new PrintWriter(s"reviews${listBuilder.result().size}.txt") { // open new file
      listBuilder.result().foreach(x => write(x + "\n")) // write distinct list to file
      //iLiveToServe.distinct.sorted.map(ssplit(_)).flatten.foreach(x => write(x + "\n")) // write distinct list to file
      close // close file
    }
  }

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
    println(listBuilder.result().size)
    listBuilder.result()
    /*val client = HttpClient(ElasticsearchClientUri("localhost", 9200)) // new client
    implicit val timeout = Duration(10, "seconds") // is the timeout for the SearchIterator.hits method
    val listBuilder = List.newBuilder[String]
    val iterator = SearchIterator.hits(client, search("amazon_reviews_metadata").keepAlive("1m").size(50).sourceInclude("asin") query fuzzyQuery("categories",category))  // returns 100 values and blocks until the iterator gets to the last element

    client.close() // close HttpClient
    iterator.foreach(println)

    def iLiveToServe(iterator:Iterator[SearchHit]): List[String] = {
      var counter: Int = 0 // limit iterationsq

      iterator.foreach(x => { // for each element in the iterator
        if (counter < numberOfProducts) { // limit iterations
          listBuilder += x.sourceField("asin").toString
          counter += 1
        } else {

          println("results for "+ category +" " + listBuilder.result().size)
          return listBuilder.result()
        }
      })

      println("results for "+ category +" " + listBuilder.result().size)
      listBuilder.result()
    }
    iLiveToServe(iterator)*/
  }

  // MAIN
  def main(args: Array[String]): Unit = {
    val numberOfProducts:Int = 10000
    val numberOfReviews:Int = 10000
    val listOfProducts = getListOfProducts("Clothing",numberOfProducts)
    saveAmazonReviewsInAFile(numberOfReviews,listOfProducts)

  }
}
