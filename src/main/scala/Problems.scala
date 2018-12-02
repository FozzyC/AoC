import java.io.FileReader
import java.io.BufferedReader
import scala.collection.mutable.Set

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

}
