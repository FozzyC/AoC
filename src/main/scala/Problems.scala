import java.io.FileReader
import java.io.BufferedReader
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.io.Source._
import scala.collection.mutable.{ArrayBuffer, Set}

object Problems {


  def day1_1(inputFile: String, startingVal: Int): Int ={
    var counter = startingVal
    //read the input file line by line
      val f = new BufferedReader(new FileReader(inputFile))
     while(f.ready()){
       //parse line as int.
       val number = Integer.parseInt(f.readLine())
       counter = (counter + number)
     }
    return counter
  }
  def day1_2(inputFile: String): Int = {
    var countSet = scala.collection.mutable.Set[Int]()
    var frequency = 0
    //read the input file line by line
    while (true) {
      val f = new BufferedReader(new FileReader(inputFile))
      while (f.ready()) {
        //parse line as int.
        val number = Integer.parseInt(f.readLine())
        frequency = frequency + number
        if(countSet.contains(frequency)){
          return frequency
        }
        countSet.add(frequency)
      }
      f.close()
    }
    return 0
  }

  def day2_1(inputFile: String): Int = {
    var twoLetter = 0
    var threeLetter = 0

    var letters:scala.collection.mutable.Map[Char,Int] = scala.collection.mutable.Map()
    val f = new BufferedReader(new FileReader(inputFile))
    while (f.ready()) {
      val line = f.readLine()
      for(x:Char <- line){
        letters += (x -> (letters.getOrElse(x,0) + 1))
      }
      //check for exactly 2 or 3 values
      if(letters.values.exists(_ == 2)){
        twoLetter += 1
      }
      if(letters.values.exists(_ == 3)){
        threeLetter += 1
      }
      //clear the letters map
      letters.clear()
    }
    return twoLetter * threeLetter
  }
  def day2_2(inputFile: String): String = {
    var stringSet: scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

    val f = new BufferedReader(new FileReader(inputFile))
    while (f.ready()) {
      val line = f.readLine()
        stringSet.add(line)
      }
    while(true){
        stringSet.tail.foreach(u => if((u zip stringSet.head).count{case (x,y) => x != y} == 1 ){
        //one char doesn't match
         //return u.replace(u.diff(stringSet.head),"") // REPLACING ALL OF THIS LETTER :(
          var x = (u zip stringSet.head).indexWhere{case (x,y) => x != y}
          return u.substring(0,x) + u.substring(x+1,u.length)
        println("test")
      })
      stringSet.remove(stringSet.head)
      }
    return ""
  }

  def day3_1(inputFile: String): Int = {
    var claimList: List[Claim] = List[Claim]()
    val f = new BufferedReader(new FileReader(inputFile))
    while (f.ready()) {
      val line = f.readLine()
      val id = Integer.parseInt(line.substring(1,line.indexOf("@")).trim)
      val x = Integer.parseInt(line.substring(line.indexOf("@")+1,line.indexOf(",")).trim)
      val y = Integer.parseInt(line.substring(line.indexOf(",")+ 1,line.indexOf(":")).trim)
      val w = Integer.parseInt(line.substring(line.indexOf(":")+1,line.indexOf("x")).trim)
      val h = Integer.parseInt(line.substring(line.indexOf("x")+1,line.length).trim)
      claimList = Claim(id,x,y,w,h, x+w, y+h) :: claimList
    }
    //var arrayCounter :ArrayBuffer[Int] = new ArrayBuffer[Int](claimList.max.maxX,claimList.max.maxY) // uses max'es to define array size.
    val arrayCounter: Array[Array[Int]] = Array.ofDim[Int](claimList.maxBy(x => x.maxX).maxX+1,claimList.maxBy(x => x.maxY).maxY+1)
    for (c <- claimList){
      var w = 0
      var h = 0
      while(h < c.height){
        while(w < c.width)
          {
            arrayCounter(c.x +w)(c.y+h) += 1
            w +=1
          }
        w = 0
        h += 1
      }
    }
    //see count of array items that have value of 2 or more.
    var finalResult = 0
    arrayCounter.foreach(x => x.foreach(y => if(y > 1){finalResult += 1}))
    return finalResult
  }
  def day3_2(inputFile: String): Int = {
    var claimList: List[Claim] = List[Claim]()
    val f = new BufferedReader(new FileReader(inputFile))
    while (f.ready()) {
      val line = f.readLine()
      val id = Integer.parseInt(line.substring(1,line.indexOf("@")).trim)
      val x = Integer.parseInt(line.substring(line.indexOf("@")+1,line.indexOf(",")).trim)
      val y = Integer.parseInt(line.substring(line.indexOf(",")+ 1,line.indexOf(":")).trim)
      val w = Integer.parseInt(line.substring(line.indexOf(":")+1,line.indexOf("x")).trim)
      val h = Integer.parseInt(line.substring(line.indexOf("x")+1,line.length).trim)
      claimList = Claim(id,x,y,w,h, x+w, y+h) :: claimList
    }
    //var arrayCounter :ArrayBuffer[Int] = new ArrayBuffer[Int](claimList.max.maxX,claimList.max.maxY) // uses max'es to define array size.
    val arrayCounter: Array[Array[Int]] = Array.ofDim[Int](claimList.maxBy(x => x.maxX).maxX+1,claimList.maxBy(x => x.maxY).maxY+1)
    for (c <- claimList){
      var w = 0
      var h = 0
      while(h < c.height){
        while(w < c.width)
        {
          arrayCounter(c.x +w)(c.y+h) += 1
          w +=1
        }
        w = 0
        h += 1
      }
    }

    for(i<-0 to arrayCounter.length)
      {
        for(c<-0 to claimList.maxBy(x => x.maxY).maxY)
          {
            if (arrayCounter(i)(c) == 1){ //if its one this particular square did not overlap
              if(claimList.count(x => x.x == i && x.y == c) == 1){ //there is an origin at this point
                val claim = claimList.filter(x => x.x == i && x.y == c)(0) //check no parts of the claim at this origin overlap with any other.
                var w = 0
                var h = 0
                var claimDoesNotOverLap = true
                while(h < claim.height){
                  while(w < claim.width)
                  {
                    if(arrayCounter(claim.x +w)(claim.y+h) > 1){
                      {
                        claimDoesNotOverLap = false
                      }
                    }
                    w +=1
                  }
                  w = 0
                  h += 1
                }
                if(claimDoesNotOverLap == true){return claim.id}
              }
            }
          }
      }
    return 0
  }

  def day4_1(inputFile: String): Int = {
    val lines = fromFile(inputFile).getLines
    val sortedList = lines.toList.sortBy(s => new SimpleDateFormat("yyyy-MM-dd hh:mm").parse("2018"+s.substring(5,s.indexOf("]"))))
    // now it's in chronological order
    var i = 0
    var guardList : List[Awake] = List[Awake]()
    while(i < sortedList.length)
      {
        if (sortedList(i).contains("#")){
          val id = Integer.parseInt(sortedList(i).substring(sortedList(i).indexOf("#")+1,sortedList(i).indexOf("begins")).trim)
          val date = sortedList(i).substring(sortedList(i).indexOf("[")+1,sortedList(i).indexOf(" "))
          var asleepList : List[Int] = List.empty[Int]
          i += 1
          while(i < sortedList.length && !sortedList(i).contains("#"))
            {
              val sleep :Int = Integer.parseInt(sortedList(i).substring(15,17))
              val awaken :Int = Integer.parseInt(sortedList(i+1).substring(15,17)) -1
              val minsAsleep = (sleep to awaken).toList
              //add asleep times
              asleepList = minsAsleep ::: asleepList
              i += 2
            }
            guardList = Awake(id,date,asleepList) :: guardList
        }
      }
    val guardListSummed = guardList.map(x => (x.guardID,x.asleepMins.length))
    //guardListSummed.foreach(x => println(x._1 + "," + x._2))
    val worstGuard = guardListSummed.maxBy(x => x._2)._1 //AM i getting the right guard?
    val justThisGuard = guardList.filter(x => x.guardID == worstGuard)
    val mostMin = justThisGuard.flatMap(x => x.asleepMins).groupBy(identity)
    val mostMin2 = mostMin.maxBy(x => x._2.size)._2.size
    val mostMinsFinal = mostMin.filter(x => x._2.size == mostMin2).maxBy(x => x._1)._1
    //mostMin.foreach(x => println(x))
    return worstGuard * mostMinsFinal
  }
}
case class Claim(id: Int,x: Int,y: Int,width: Int,height: Int, maxX : Int, maxY: Int)
case class Awake(guardID: Int, date: String, asleepMins:List[Int])