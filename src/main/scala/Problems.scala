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


}
