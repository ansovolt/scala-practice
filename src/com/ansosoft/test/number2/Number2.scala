package com.ansosoft.test.number2

import scala.io.Source

/**
  * Created by asochal on 1/9/2018.
  */
object Number2 extends App{


  val bb = new BlobBuilder()
  bb.build()
  val blobs = bb.blobs
  for (b <- blobs){
    b.printBlob()
    println()
  }

}

class BlobBuilder {

  var totalRec:Int = -1
  var currRec:Int = 0
  var currBlob:Blob = null

  var blobs:List[Blob] = List()

  def build() ={

    for (l <- Source.fromFile("C:\\Users\\asochal\\projects\\git\\playground\\scala\\src\\com\\ansosoft\\test\\number2\\input.dat").getLines()){
      if (totalRec == -1 ){
        totalRec = l.toInt
      }
      else if (currBlob == null){
        currBlob = new Blob()
        blobs = currBlob :: blobs
        val chunks = l.split(" ")
        currBlob.rows = chunks(0).toInt
        currBlob.cols = chunks(1).toInt
        currBlob.testCount = chunks(2).toInt
        currBlob.initBlob(currBlob.rows,currBlob.cols)
      } else {
        if (l.contains(".") || l.contains("*")){

          l.toCharArray.foreach((c:Char) => {
            var element:Int = 0
            if (c == '*'){
              element = 1
            }
            currBlob.addElement(l,element)
          })

        }
        else {
          if ( currBlob.testCases.size < currBlob.testCount){
            val chunks = l.split(" ")
            currBlob.testCases = (chunks(0).toInt,chunks(1).toInt) :: currBlob.testCases
          }
          else {

            currBlob = new Blob()
            blobs = currBlob :: blobs
            val chunks = l.split(" ")
            currBlob.rows = chunks(0).toInt
            currBlob.cols = chunks(1).toInt
            currBlob.testCount = chunks(2).toInt
            currBlob.initBlob(currBlob.rows,currBlob.cols)
          }
        }
      }
    }
  }
}

class Blob {

  var rows:Int = -1
  var cols:Int = -1
  var testCount:Int = -1
  var testCases:List[(Int,Int)] = List()
  var blob = Array.ofDim[Int](1,1)
  var currentRow:Int = 0
  var currentCol:Int = 0

  def initBlob(rows:Int, cols:Int) = {
    blob = Array.ofDim[Int](rows,cols)
  }

  def addElement(l:String,e:Int) = {
//    if (currentCol == 0){
//      println()
//      println(l)
//    }
//    println(s"adding $e to [$currentRow,$currentCol]")
    blob(currentRow)(currentCol) = e
    if (currentCol + 1 < cols){
      currentCol = currentCol + 1
    }
    else {
      currentCol = 0
      if (currentRow + 1 < rows){
        currentRow = currentRow + 1
      }
    }
  }

  def printBlob() = {
    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        print(blob(r)(c))
      }
      println()
    }
    println("testcases:")
    for (tc <- testCases){
      println(s"$tc")
    }
  }

  def findBlobSize(row:Int,col:Int) :String = {
    var retVal = "NO BLOB"



    return retVal
  }


  def findNeighbours(row:Int, col:Int):List[(Int,Int)] = {
    var neighbours = List()



    return neighbours
  }

}