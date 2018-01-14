package com.ansosoft.practice1.Number5
import scala.io.Source

object Number5 extends App {
  val f = new FileReader("C:\\Users\\beti\\ansosoft\\projects\\scala-practice\\src\\com\\ansosoft\\practice1\\number5\\family.dat")
  f.read()
  val ft = new GenTree()
  ft.addRelationships(f.rels)
  ft.reducePeerGens()
  ft.printGens()
}

case class Gen(id:Int, var members:List[String]){
  var parentGenIds:List[Int] = List()
  var del:Boolean = false
  var printed:Boolean = false

  def membersAsString() :String = {
    var retVal = ""
    for (m <- members){
      retVal = retVal + " " + m
    }
    return retVal
  }
  def hasMember(name:String): Boolean = {
    for (m <- members){
      if (m.equals(name)){
        return true
      }
    }
    return false
  }
}

class GenTree {
  var generations:List[Gen] = List()
  var gId:Int = 0

  def addRelationships(rels:List[Relationship]):Unit = {
    for (rel <- rels){
      addRelationship(rel)
    }
  }
  def addRelationship(r:Relationship):Unit = {
    val g1 = getGen(r.src)
    val g2 = getGen(r.dest)
    if (r.descNoun == "DAUGHTER" ||r.descNoun == "SON" ){
      createSubGen(g1,g2)
    }
    else if (r.descNoun == "MOTHER" ||r.descNoun == "FATHER"){
      createSubGen(g2,g1)
    }
    else if (r.descNoun == "SISTER" ||r.descNoun == "BROTHER"){
      createPeerGen(g1,g2)
    }
    else if (r.descNoun == "HUSBAND" ||r.descNoun == "WIFE"){
      createPeerGen(g1,g2)
    }
  }


  def reducePeerGens() = {
    val idsBeforeReduce = generations.filter(_.parentGenIds.isEmpty).map(
      (x:Gen) => x.id
    )

    val nGen = getEmptyGen()
    generations.filter((x:Gen) => {x.parentGenIds.isEmpty && x.id != nGen.id}).foreach(
      (x:Gen) => {
        nGen.members = nGen.members ::: x.members
        x.del = true
      }
    )
    generations = generations.filterNot(_.del)

    generations.filter(!_.parentGenIds.isEmpty).foreach(
      (x:Gen) => {
        val isIn:Boolean = x.parentGenIds.forall(idsBeforeReduce.contains)
        if (isIn) {
          x.parentGenIds = List(nGen.id)
        }
      }
    )
  }

  def createSubGen(g1:Gen,g2:Gen):Unit = {
    g1.parentGenIds = g2.id :: g1.parentGenIds
  }

  def createPeerGen(g1:Gen,g2:Gen):Unit = {
    if (g1 != g2){
      g1.members = g1.members ::: g2.members
      generations = generations.filterNot(_ == g2)
    }
  }

  def getGen(name:String) : Gen= {
    var retVal:Gen = null
    for (gen <- generations){
      if (gen.hasMember(name)){
        retVal = gen
      }
    }
    if (retVal == null){
      retVal = new Gen(getId() ,List(name))
      generations = retVal :: generations
    }
    return retVal
  }
  def getEmptyGen() : Gen= {
    var retVal:Gen = null
    retVal = new Gen(getId() ,List())
    generations = retVal :: generations
    return retVal
  }

  def getId():Int = {
    gId = gId+1
    return gId
  }

  def printGens():Unit = {

    def conv(n:Int)=n+{if(n%100/10==1)"th"else(("thstndrd"+"th"*6).sliding(2,2).toSeq(n%10))}

    var degree:Int = 1
    var parentIds:List[Int] = List()

    //1st generation
    generations.filter(_.parentGenIds.isEmpty).foreach((x: Gen) => {
      val prefix = conv(degree)
      println(s"$prefix Generation: " + x.membersAsString())
      degree = degree + 1
      parentIds = x.id :: parentIds
      x.printed = true
    })

    var allPrinted  = false
    while (!allPrinted) {

      allPrinted = generations.filter(!_.printed).isEmpty

      generations.filter(!_.parentGenIds.isEmpty).foreach(
        (x: Gen) => {
          val isIn: Boolean = x.parentGenIds.forall(parentIds.contains)
          if (isIn) {
            val prefix = conv(degree)
            println(s"$prefix Generation: " + x.membersAsString())
            degree = degree + 1
            parentIds = List()
            parentIds = x.id :: parentIds
            x.printed = true
          }
        }
      )
    }
  }
}


case class Relationship(src:String,descNoun:String,dest:String)

class FileReader (filePath:String) {
  var rels = List[Relationship]()
  def read() = {
    for (line <- Source.fromFile(filePath).getLines()){
      val chunks=line.split(" ")
      val r = Relationship(chunks(0),chunks(1),chunks(2))
      rels = r :: rels
    }
  }
}
