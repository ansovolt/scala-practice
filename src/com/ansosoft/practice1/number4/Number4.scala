package com.ansosoft.practice1.number4

import scala.io.Source

object Number4 extends App {

  var lineCounter:Int = 0
  var size1:Int = -1
  var size2:Int = -1
  var sheet1:Sheet = null
  var sheet2:Sheet = null
  for (l <- Source.fromFile("C:\\Users\\beti\\ansosoft\\projects\\scala-practice\\src\\com\\ansosoft\\practice1\\number4\\input.dat").getLines()) {
    if (sheet1 == null) {
      val chunks = l.split(" ")
      size1 = chunks(0).toInt
      size2 = chunks(1).toInt
      sheet1 = new Sheet()
      sheet2 = new Sheet()
    }
    else if (lineCounter > 0 && lineCounter <= size1 ) {
      sheet1.add(l)
    } else {
      sheet2.add(l)
    }
    lineCounter = lineCounter + 1
  }

  //println(sheet1.names)
  //println(sheet2.names)

  println("NAMES IN SPREADSHEET 1 THAT ARE NOT IN SPREADSHEET 2")
  sheet1.names.filter((n:String)=>{if (!sheet2.names.contains(n)) true else false }).sorted.foreach(println(_))

  println("NAMES IN SPREADSHEET 2 THAT ARE NOT IN SPREADSHEET 1")
  sheet2.names.filter((n:String)=>{if (!sheet1.names.contains(n)) true else false }).sorted.foreach(println(_))


}

class Sheet {
  var names:List[String] = List()
  def add(n:String) ={
    names = n :: names
  }
}
