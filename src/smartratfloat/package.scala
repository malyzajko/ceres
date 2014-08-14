package ceres

package object smartratfloat {

  var doubleFormat = "%1.16e"

  var maxNoiseCount = 42

  var maxNoise = 0

  val packingThreshold = math.pow(10, -32)

  var printComparisonFailure = false

  var failMessage = ""
}