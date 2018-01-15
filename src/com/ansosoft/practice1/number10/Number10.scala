package com.ansosoft.practice1.number10

import scala.io.Source

object Number10 extends App {

  for (l <- Source.fromFile("C:\\Users\\beti\\ansosoft\\projects\\scala-practice\\src\\com\\ansosoft\\practice1\\number10\\input.dat").getLines()) {
    val chunks = l.split(" ")
    for (c <- chunks){
      val x = c.toInt
      //print(s"$c ")
      println(findAllOnesMultiple(x))
    }
  }

  def findAllOnesMultiple(x:Int):String = {
    var retVal = "INVALID INPUT"

    def check(x:Long, ones:String):Boolean ={
      var retVal:Boolean = false
      val o = ones.toLong
      if (o % x == 0) retVal = true
      return retVal
    }

    if ( x % 2 == 0 || x % 5 == 0){
    }
    else {
      var ones:String = "1"
      while (!check(x,ones)){
        ones = ones + "1"
      }
      retVal = ones.size.toString
    }
    return retVal
  }
}