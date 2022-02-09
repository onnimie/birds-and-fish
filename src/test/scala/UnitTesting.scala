import scala.math.{Pi, atan, sqrt}

object UnitTesting extends App {

  /**
   * With this class I will do unit tests to some implementations
   *
   */

  def testAngle() = {
    println("\n" + "------ angle testing ----")

    var test = new Vector2D(2, 3)
    println(test.angle)
    println("should equal close to " + atan(3.0 / 2) + "\n")

    test = new Vector2D(-4, 5)
    println(test.angle)
    println("should equal close to " + (Pi - atan(5.0 / 4)) + "\n")

    test = new Vector2D(-1, -3)
    println(test.angle)
    println("should equal close to " + (Pi + atan(3.0 / 1)) + "\n")

    test = new Vector2D(2, -2)
    println(test.angle)
    println("should equal close to " + (2 * Pi - atan(2.0 / 2)) + "\n")
  }

  def testMagnitude() = {
    println("----- magnitude testing -----")
    var test = new Vector2D(23, 45)
    println(test.magnitude)
    println("should equal close to " + sqrt(23 * 23 + 45 * 45) + "\n")

    test = new Vector2D(-13, 4)
    println(test.magnitude)
    println("should equal close to " + sqrt(13 * 13 + 4 * 4) + "\n")

    test = new Vector2D(-6, -19)
    println(test.magnitude)
    println("should equal close to " + sqrt(6 * 6 + 19 * 19) + "\n")

    test = new Vector2D(39, -999)
    println(test.magnitude)
    println("should equal close to " + sqrt(39 * 39 + 999 * 999) + "\n")
  }

  def testNormalize() = {
    println("----- normalizing testzone ------" + "\n")
    var test = new Vector2D(100, 314)
    var norm = test.normalize
    println("100,314" + " mag(" + test.magnitude + ")" + " --> " + norm.magnitude + "\n")

    test = new Vector2D(-165, 2)
    norm = test.normalize
    println("-165,2" + " mag(" + test.magnitude + ")" + " --> " + norm.magnitude + "\n")

    test = new Vector2D(-1111, -786)
    norm = test.normalize
    println("-1111,-786" + " mag(" + test.magnitude + ")" + " --> " + norm.magnitude + "\n")

    test = new Vector2D(17, -991)
    norm = test.normalize
    println("17,-991" + " mag(" + test.magnitude + ")" + " --> " + norm.magnitude + "\n")

    test = new Vector2D(10090, 31412)
    norm = test.normalize
    println("10090,31412" + " mag(" + test.magnitude + ")" + " --> " + norm.magnitude + "\n")

    test = new Vector2D(-13312, 5476)
    norm = test.normalize
    println("-13312,5476" + " mag(" + test.magnitude + ")" + " --> " + norm.magnitude + "\n")

    test = new Vector2D(7711, -88781)
    norm = test.normalize
    println("7711,-88781" + " mag(" + test.magnitude + ")" + " --> " + norm.magnitude + "\n")
  }

  def testUnitVectorFromAngle() = {
    println("---- testing unitVector from angle -----" + "\n")
    val testAngles: Array[Float] = Array((Pi / 4).toFloat, (Pi / 6).toFloat, 1.23334f, (1.9 * Pi).toFloat, Pi.toFloat, 0f, 2.142f, 4.12345f)
    // each angle needs to be between 0 and 2pi radians
    for (angle <- testAngles) {
      println("angle: " + angle)
      val test = Vector2D.unitVectorFromAngle(angle)
      println(s"$test, mag: ${test.magnitude}, angle: ${test.angle}")
      println()
    }
  }

  def testRotatedBy() = {
    println("----- testing rotatedBy(angle) -------" + "\n")
    // angles can be negative and any value
    val testAngles: Array[Float] = Array(-1.3f, Pi.toFloat, (2 * Pi).toFloat, 0.1f, 16f)
    val testVectorXYs: Array[(Float, Float)] = Array((1f, 1f), (1f, 0f), (13f, 0.4f), (-1f, 0f), (-0.1f, 11f))

    for (xy <- testVectorXYs) {
      val vector = new Vector2D(xy._1, xy._2)
      for (angle <- testAngles) {
        println("" + vector + " rotated by " + angle)
        val rotvec = vector.rotatedBy(angle)
        println(s"${rotvec.magnitude} should equal original: ${vector.magnitude}")
        println(s"new vs old angle: ${rotvec.angle} , ${vector.angle}")
        println(s"${rotvec.angle} should equal to around ${(vector.angle + angle) % (2 * Pi).toFloat}")
        println()
      }
    }
  }

  //testAngle() //works
  //testMagnitude() //works
  //testNormalize() //works
  //testUnitVectorFromAngle()
  testRotatedBy()
}
