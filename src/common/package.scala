package ceres

package object common {

  def niceDoubleString(d: Double) = {
    var numString = "%.16g".format(d)
    //println("numString: " + numString)
    var continue = (numString.length > 1)
    while(continue) {
      if (numString.last == '0') {
        numString = numString.dropRight(1)
        //println("numString after drop: " + numString)
        continue = true
      }
      else {
        continue = false
      }
    }
    if (numString.last == '.') numString.dropRight(1)
    else numString
  }

}
