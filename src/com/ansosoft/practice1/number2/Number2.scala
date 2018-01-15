package com.ansosoft.practice1.number2

import scala.collection.mutable
import scala.io.Source

/**
  * Created by asochal on 1/9/2018.
  */
object Number2 extends App{


  val bb = new BlobBuilder()
  bb.build()
  val blobs = bb.blobs
  for (b <- blobs.reverse){
    //b.printBlob()
    //println(b.findNeighbours(1,2))
    //println(b.findBlobSize(0,6))
    print(b.runTests())
    //println()
  }

}

class BlobBuilder {

  var totalRec:Int = -1
  var currRec:Int = 0
  var currBlob:Blob = null

  var blobs:List[Blob] = List()

  def build() ={

    for (l <- Source.fromFile("C:\\Users\\beti\\ansosoft\\projects\\scala-practice\\src\\com\\ansosoft\\practice1\\number2\\input.dat").getLines()){
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
    var count:Int = 0
    var retVal = "NO BLOB"
    var processed:List[(Int,Int)] = List()
    var pending:mutable.Queue[(Int,Int)] = mutable.Queue()

    if (isIn(row,col) && blob(row)(col) == 1){
      pending.enqueue((row,col))
      while (!pending.isEmpty) {
        val working = pending.dequeue()
        //println(s"working: $working")
        processed = working :: processed
        val n = findNeighbours(working._1,working._2)
        val f = n.filter(( e:(Int,Int)) => {
          if ( processed.contains(e) ) false else true
        })
        f.foreach( (e:(Int,Int)) => {
          if (!pending.contains(e)) pending.enqueue(e)
          count = count + 1
        } )
      }
    }

    if (!processed.isEmpty){
      retVal = processed.size.toString
    }

    return retVal;
  }


  def findNeighbours(row:Int, col:Int):List[(Int,Int)] = {
    var neighbours = List[(Int,Int)]()

    if (isIn(row,col-1) && blob(row)(col-1) == 1){
      neighbours = (row,col-1) :: neighbours
    }
    if (isIn(row-1,col-1) && blob(row-1)(col-1) == 1){
      neighbours = (row-1,col-1) :: neighbours
    }
    if (isIn(row-1,col) && blob(row-1)(col) == 1){
      neighbours = (row-1,col) :: neighbours
    }
    if (isIn(row-1,col+1) && blob(row-1)(col+1) == 1){
      neighbours = (row-1,col+1) :: neighbours
    }
    if (isIn(row,col+1) && blob(row)(col+1) == 1){
      neighbours = (row,col+1) :: neighbours
    }
    if (isIn(row+1,col+1) && blob(row+1)(col+1) == 1){
      neighbours = (row+1,col+1) :: neighbours
    }
    if (isIn(row+1,col) && blob(row+1)(col) == 1){
      neighbours = (row+1,col) :: neighbours
    }
    if (isIn(row+1,col-1) && blob(row+1)(col-1) == 1){
      neighbours = (row+1,col-1) :: neighbours
    }
    return neighbours
  }

  def isIn(row:Int,col:Int):Boolean = {
    var isIn = false;
    if (row >= 0 && col >= 0 && row < blob.length && col < blob(0).length ){
      isIn = true
    }
    return isIn
  }

  def runTests():String = {
    var results:String = ""
    for (tc <- testCases.reverse){
      results = results + findBlobSize(tc._1 -1 ,tc._2 - 1)
      results = results + "\n"
    }

    return results
  }

}