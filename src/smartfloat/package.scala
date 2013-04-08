package ceres

package object smartfloat {
  /*
    #####################     Helper     #########################
  */
  //math.Pi is correctly rounded (rdd down, by inspection) so we only need to round up
  val Pi_up = java.lang.Math.nextUp(math.Pi)
  val Pi_down = math.Pi
  val twoP = 2.0*math.Pi

  /**
   * Compares two affine forms.
   * Returns true, only if this is true within the error bounds.
   */
  def smaller(x: AffineForm, y: AffineForm): Boolean = {
    val diff = (x - y).interval
    return diff.xhi < 0.0
  }

  var maxNoise = 0

  var addCount = 0L
  var subCount = 0L
  var multCount = 0L
  var unaryCount = 0L
  var trigCount = 0L

  def clearCounts = {
    addCount = 0L
    subCount = 0L
    multCount = 0L
    unaryCount = 0L
    trigCount = 0L
    maxNoise = 0
  }

  def printCounts = {
    println("add: " + addCount + ", sub:" + subCount + ", mult:"+multCount +
      ",unary:"+unaryCount + ", trig:"+trigCount +
      ", total:" + (addCount+subCount+multCount+unaryCount) + ", maxNoise:" + maxNoise)
  }

  /*
    #####################     OPTIONS     #########################
  */
  /** If true, will print a warning each time a comparison fails.*/
  var printComparisonFailure = false

  /** String that will be printed when a comparison fails, to help with debugging.*/
  var failMessage = ""

  /** If true, roundoffs for SmartAForm and ParamAForm will be computed with smartinterval, instead of interval.*/
  var smartRoundoffComputation = false

  /** If true, debugging information will be printed. */
  var printDebug = false

  /** If true, will print which operations have failed, e.g. due to division of zero. */
  var printFailed = false

  /** If true, will print info about how many noise terms were compacted to how many. */
  var printPackingInfo = false

  /** Limit on the number of noise terms before smartintervals are computed. */
  var smartQueueLimit = 10

  /** Limit on number of noise symbols. */
  var maxNoiseCount = 42      //Maximum number of noise symbols we want to accumulate.

  /** Threshold packing is computed as a * avrg + b * stdDev. This is the parameter a. */
  var packingFactor = 1.0

  /** As packingFactor, but will be used when packing queue for smartinterval computation. */
  var smartPackingFactor = 1.0

  /** Threshold packing is computed as a * avrg + b * stdDev. This is the parameter b. */
  var packingAvrgScale = 1.0

  /** As packingAvrgScale, but will be used when packing queue for smartinterval computation. */
  var smartPackingAvrgScale = 1.0

  /** Formatting to be used when printing doubles. */
  var doubleFormat = "%1.4e"

  /** All noise terms smaller than this threshold will be packed. */
  var smartPackingThreshold = math.pow(10, -20) //customizable threshold

  /** How much higher order information do we keep. */
  var comesFromArraySize = 8

  val packingThreshold = math.pow(10, -32) //smaller numbers should be internal errors



}
