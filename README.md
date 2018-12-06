# AoC
AoC 2018
input : String = "abcbabcBcbabcBacbbcbACBcabBCABcabbcabCBBcCaaaAb"
for(c <- input){
  
}

def runThroughList(in:ArrayBuffer) : String{
  val i = 0
  while(i < stringArray.length)
  {
    if(stringArray(i).toUpper == stringArray(i+1).toUpper){
      if(stringArray(i) != stringArray(i+1)){
        //remove these two items and re-run function
        stringArray.remove(i)
        stringArray.remove(i+1)
        runThroughList(stringArray)
      }
    }
  }
  return stringArray.toString //or may need to fold? 
}
