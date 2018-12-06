# AoC
AoC 2018
input : String = "abcbabcBcbabcBacbbcbACBcabBCABcabbcabCBBcCaaaAb"
for(c <- input){
  
}

def runThroughList(in:String) : String{
  var stringArray = in.toArray
  val i = 0
  while(i < stringArray.length)
  {
    if(stringArray(i).toUpper == stringArray(i+1).toUpper){
      if(stringArray(i) != stringArray(i+1)){
        //remove these two items and re-run function
      }
    }
  }
  return stringArray.toString //or may need to fold? 
}
