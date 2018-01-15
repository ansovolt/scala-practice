package com.ansosoft.practice1.number9

import scala.io.Source

object Number9 extends App{

  val divider = new CdDivider

  var cnt = -1
  for (l <- Source.fromFile("C:\\Users\\beti\\ansosoft\\projects\\scala-practice\\src\\com\\ansosoft\\practice1\\number9\\input.dat").getLines()) {
    if (cnt == -1){
      cnt = l.toInt
    }
    else {
      val chunks = l.split(" ")
      val ss: Int = chunks(chunks.length - 1).toInt
      val mm: Int = chunks(chunks.length - 2).toInt
      var title: String = ""
      val tchunks = chunks.splitAt(chunks.length - 2)._1
      for (c <- tchunks) title = title + c + " "
      title = title.trim
      divider.addSong(Song(title,mm,ss))
    }
  }

  divider.print()

}

case class Song(title:String, mm:Int, ss:Int)
case class CD() {
  val maxMin = 20
  var songs:List[Song] = List()
  var totalTime:(Int,Int) = (0,0)
  var isFull:Boolean = false

  def setNewTime(nt:(Int,Int)) ={
    totalTime = nt
  }
  def getNewTime(mm:Int,ss:Int) : (Int,Int) = {
    val rem = (totalTime._2 + ss) % 60
    val whole = (totalTime._2 + ss) / 60
    val nm = totalTime._1 + whole + mm
    val ns = rem
    return (nm,ns)
  }

  def addSong(s:Song):Boolean = {
    var retVal = true
    val nt = getNewTime(s.mm,s.ss)
    if ((nt._1 > maxMin) || (nt._1 == maxMin && nt._2 > 0)) {
      retVal = false
      isFull = true
    }
    else {
      songs = s :: songs
      setNewTime(nt)
    }
    return retVal
  }
}

class CdDivider {
  var cdList:List[CD] = List()

  def addSong(s:Song) ={
    var cd:CD = null
    val available = cdList.filter(!_.isFull)
    if (!available.isEmpty){
      cd = cdList.filter(!_.isFull)(0)
    }
    else {
      cd = new CD()
      cdList = cd :: cdList
    }

    if (!cd.addSong(s)){
      cd = new CD()
      cdList = cd :: cdList
      cd.addSong(s)
    }

  }

  def print() = {
    //println(cdList.size)

    var cnt = 1
    for (cd <- cdList.reverse){
      println(s"CD #$cnt")
      //cd.songs.foreach((s:Song) => println(s.title))
      cd.songs.foreach((s:Song) => println(s.title+" "+s.mm+" "+s.ss))
      var minStr = ""
      if (cd.totalTime._2 < 10)  minStr = "0"+cd.totalTime._2 else minStr = cd.totalTime._2.toString
      println(cd.totalTime._1+":"+ minStr)
      println()
      cnt = cnt + 1
    }

  }

}



