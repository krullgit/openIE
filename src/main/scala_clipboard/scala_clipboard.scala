import com.sksamuel.elastic4s.http.ElasticDsl.{bulk, get, indexInto, search}
import com.sksamuel.elastic4s.http.search.{SearchHit, SearchResponse}
import de.tu_berlin.dima.code.HttpClientExampleApp.{client, flatten, getFieldHelp}
import org.elasticsearch.action.support.WriteRequest.RefreshPolicy


// insert into scala
/*client.execute {
  bulk(
    indexInto("myindex" / "mytype").fields("country" -> "Mongolia", "capital" -> "Ulaanbaatar"),
    indexInto("myindex" / "mytype").fields("country" -> "Namibia", "capital" -> "Windhoek")
  ).refresh(RefreshPolicy.WAIT_UNTIL)
}.await*/

/*// einfache Anfrage mit Key Value
search("rss"/"rss3").matchQuery("uri.string", "http://bauarbeiten.bahn.de/fernverkehr/Linie/IC_56-Norddeich_Emden-Oldenburg-Bremen-Hannover-Magdeburg-Leipzig_Cottbus/30#4194252c51f30e3a2a9401d77689ce1ea38b61bd")
//oder
get("AV8qANb3kw-HPYD6zM_E").from("testgruppierung/testgruppierung1")*/

/*def printEntityTypes(x: SearchHit): String = {

    // TEST if field is available
    try {
      toOption(x.sourceField("relationMentions"))
    } catch {
      case e: Exception => {
        return ""
      }
    }
    // GET the field
    toOption(x.sourceField("relationMentions")) match {

      case Some(x) => x.asInstanceOf[Map[Any, Any]] match {
        case i: Map[Any, Any] => i.get("array") match {
          case Some(i) => i match {
            case i: List[Any] => {
              if (i.isEmpty == false) {
                i.head.asInstanceOf[Map[Any, Any]] match {
                  case i: Map[Any, Any] => i.get("name").get.toString
                  case _ => println("Sorry4") // do nothing
                    ""
                }
              } else ""
            }
            case _ => println("Sorry3")
              ""
          }
          case _ => println("Sorry2")
            ""
        }
        case _ => println("Sorry1")
          ""
      }
      case None => println("Sorry0")
        ""
    }
  }*/

/*for (j <-0 until path.size-1) {
        y.asInstanceOf[Map[Any, Any]] match {
          case i: Map[Any, Any] => i.get(path(j)) match {
            case Some(i) => i match{
              case i: List[Any] => {
                if (i.isEmpty == false) {
                  y = i.last
                } else y = i
              }
              case _ => println("Sorry3")
                return ""
            }
            case _ => println("Sorry2")
              return ""
          }
          case _ => println("Sorry1")
          return ""
        }
    }*/


/*return {
  y.asInstanceOf[Map[Any, Any]] match {
    case i: Map[Any, Any] => i.get(path.last) match {
      case Some(i) => {
        return i.toString
      }
      case _ => println("Sorry4")
      return ""
    }
  }
}*/

/*val resp2 = client.execute { // get all conceptMentions (which has the relationMentions)
    multi(
      for (i <- chosenTriggers.indices ) yield {
        search("rss" / "rss3")
          .regexQuery("relationMentions.array.name.keyword", chosenTriggers(i)) sourceInclude "conceptMentions.array.type" size 1000 //.*Hamburg.*Berlin.*
      }
    )
  }.await.responses*/


/*
def getField(response: List[Any], path: List[String], sourceField: String): List[String] = {

  response.head match {
    case i: SearchHit =>
      val temp = for (j <- response.indices) yield {
        getFieldHelp(response(j).asInstanceOf[SearchHit], path, sourceField)
      }
      temp.flatten.toList//.filter(i => !i.equals("")).distinct
    case j: SearchResponse =>
      val temp = for (k <- response.indices) yield {
        val temp = {
          for (m <- 0 until response.apply(k).asInstanceOf[SearchResponse].size) yield {
            getFieldHelp(response.apply(k).asInstanceOf[SearchResponse].hits.hits(m), path, sourceField)
          }
        }
        temp.toList
      }
      flatten(temp.toList).map(_.toString)//.filter(i => !i.equals("")).distinct
    case _ => List("ERROR: no SearchHit or SearchResponse")
  }
}*/

/*def printSuggestions(roleList: List[List[String]], typeList: List[List[String]], valueList: List[List[String]], chosenEntities1:List[String],chosenEntities2:List[String] ) ={

  val verbs = List("trigger")
  chosenEntities1.foreach(x => print(""+x+" "))
  print("// ")
  verbs.foreach(x => print(""+x+" "))
  print("// ")
  chosenEntities2.foreach(x => print(""+x+" "))
  print("\n\n")

  val listbuilder = List.newBuilder[String]

  for (counterTypeLists <- typeList.indices) {
    listbuilder.clear()
    for (counterType <- typeList(counterTypeLists).indices ) {
      if(chosenEntities1.contains(typeList(counterTypeLists)(counterType))){
        listbuilder += valueList(counterTypeLists)(counterType)
      }
    }
    listbuilder += " // "
    for (counterType <- typeList(counterTypeLists).indices ) {
      if(verbs.contains(typeList(counterTypeLists)(counterType))){
        listbuilder += valueList(counterTypeLists)(counterType)
      }
    }
    listbuilder += " // "

    for (counterType <- typeList(counterTypeLists).indices ) {
      if(chosenEntities2.contains(typeList(counterTypeLists)(counterType))){
        listbuilder += valueList(counterTypeLists)(counterType)
      }
    }
    listbuilder.result().foreach(x => print(""+x+" "))
    println("")
  }
}*/
