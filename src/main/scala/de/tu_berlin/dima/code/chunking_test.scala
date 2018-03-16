package de.tu_berlin.dima.code

import java.io.{FileInputStream, InputStream}
import opennlp.tools.chunker.ChunkerME

import opennlp.tools.chunker._

object chunking_test {
  def testChunking():Unit={

    var modelIn:InputStream  = null;
    var model:ChunkerModel  = null;

    try {
      modelIn = new FileInputStream("data/en-chunker.bin")
    }
    model = new ChunkerModel(modelIn)

    val chunker = new ChunkerME(model)


    val sent: Array[String] = Array[String]("Rockwell", "International", "Corp.", "'s", "Tulsa", "unit", "said", "it", "signed", "a", "tentative", "agreement", "extending", "its", "contract", "with", "Boeing", "Co.", "to", "provide", "structural", "parts", "for", "Boeing", "'s", "747", "jetliners", ".")

    val pos: Array[String] = Array[String]("NNP", "NNP", "NNP", "POS", "NNP", "NN", "VBD", "PRP", "VBD", "DT", "JJ", "NN", "VBG", "PRP$", "NN", "IN", "NNP", "NNP", "TO", "VB", "JJ", "NNS", "IN", "NNP", "POS", "CD", "NNS", ".")

    val tag: Array[String] = chunker.chunk(sent, pos)
    println(tag.zip(sent).foreach(println(_)))
  }

  def main(args: Array[String]): Unit = {
    testChunking()
  }

}
