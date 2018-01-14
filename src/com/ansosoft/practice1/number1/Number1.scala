package com.ansosoft.practice1.Number1

import java.math.RoundingMode
import java.text.DecimalFormat
import java.util.{Calendar, Date}

import scala.collection.immutable.ListMap
import scala.io.Source


object Number1 extends App {

  val tm = new TransactionManager("C:\\Users\\beti\\ansosoft\\projects\\scala-practice\\src\\com\\ansosoft\\practice1\\number1\\input.dat")
  tm.read()
  println(tm.calcBalance())
}


case class Tx(date:Date,amount:Double) {

  def getYearMonth():Long = {
    val cal = Calendar.getInstance()
    cal.setTime(date)
    cal.set(Calendar.HOUR_OF_DAY, 0)
    cal.set(Calendar.DAY_OF_MONTH, 1)
    val key = cal.getTime
    return key.getTime
  }
}

class TransactionManager (filePath:String) {
  val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
  var txs = List[Tx]()
  var minYear:Int = 3000
  var maxYear:Int = 0
  var maxYearMonths:List[Int] = List()
  var maxMonth:Int = 0

  def initFirstMonthBal(minYear:Int,maxYear:Int,maxMonth:Int):Map[Long,List[Tx]] = {

    val years = minYear to maxYear

    var retVal = Map[Long,List[Tx]]()

    for (year <- years){
      for ( m <- 1 to 12){

        val month:Int = m
        var monthStr = ""
        if (month.toInt < 10){
          monthStr = s"0$month"
        }
        else {
          monthStr = s"$month"
        }
        val dateStr = s"$year-$monthStr-01"
        val cal = Calendar.getInstance();
        cal.setTime(format.parse(dateStr));
        cal.set(Calendar.HOUR_OF_DAY, 0)

        if (year == maxYear && month > maxMonth ){

        }
        else {
          retVal = retVal + (cal.getTime.getTime -> List(Tx(cal.getTime, 0.0)))
        }

      }
    }
    return retVal
  }


  def read() = {

    def max(xs: List[Int]): Option[Int] = xs match {
      case Nil => None
      case List(x: Int) => Some(x)
      case x :: y :: rest => max( (if (x > y) x else y) :: rest )
    }



    for (line <- Source.fromFile(filePath).getLines()){
      val chunks=line.split(" ")
      val dateStr = chunks(0)
      val date:Date =  format.parse(dateStr)
      val amount = chunks(1).toDouble
      val r = Tx(date,amount)
      txs = r :: txs

      val cal = Calendar.getInstance();
      cal.setTime(date)
      val curY = cal.get(Calendar.YEAR)
      var curM = cal.get(Calendar.MONTH) + 1
      if (curY < minYear){
        minYear = curY
      }
      if (curY > maxYear){
        maxYear = curY
        maxYearMonths = List()
      }

      if (curY == maxYear){
        maxYearMonths = curM :: maxYearMonths
      }

      max(maxYearMonths) match {
        case Some(x) => maxMonth = x
        case None => maxMonth = 0
      }
      //println(r)
    }

    println(s"minYear: $minYear maxYear: $maxYear maxMonth: $maxMonth")
    //maxYearMonths.foreach(println(_))
  }

  def groupByYearMonth():Map[Long,List[Tx]] = {
    var _g = initFirstMonthBal(minYear,maxYear,maxMonth)
    val g = txs.groupBy(_.getYearMonth())
    for ((k,v) <- g){
      if (_g contains(k)){
        _g = _g + (k -> v)
      }
    }
    val smap = ListMap(_g.toSeq.sortWith( _._1 < _._1 ):_*)
    return smap
  }

  def calcBalance() : String = {

    def sum(xs: List[Double]): Double = {
      xs match {
        case x :: tail => x + sum(tail) // if there is an element, add it to the sum of the tail
        case Nil => 0 // if there are no elements, then the sum is 0
      }
    }

    var prevMonthEndBal:Double = 0.0
    val byYearMonth = groupByYearMonth()
    for ((k,v) <- byYearMonth){
      val interest =  prevMonthEndBal * 0.02
      prevMonthEndBal = prevMonthEndBal + interest
      val thisMonthBal = sum(v.map( (x:Tx) => x.amount ))
      prevMonthEndBal = prevMonthEndBal + thisMonthBal
      //println(s"key: $k value: $v this month balance: $thisMonthBal prev month end balance: $prevMonthEndBal interest: $interest")
    }

    val df = new DecimalFormat("#.##");
    df.setRoundingMode(RoundingMode.FLOOR);
    val result = (df.format(prevMonthEndBal)).toDouble
    return "$"+result
  }
}

//answer: $247341.39
