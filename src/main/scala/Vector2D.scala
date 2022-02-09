import scala.math.Pi
import scala.math.tan
import scala.math.atan
import scala.math.sin
import scala.math.cos
import scala.math.acos
import scala.math.abs
import scala.math.sqrt

class Vector2D (val x: Float, val y: Float) {
  /**
   * Vector2D is just a mathematical vector of two dimensions.
   * The velocity and position of a Boid are instances of Vector2D.
   * A Vector2D is immutable and each operation creates a new instance of Vector2D.
   */

  override def toString() = s"Vector2D($x, $y)"

  def +(another: Vector2D): Vector2D = new Vector2D(this.x + another.x, this.y + another.y)

  def -(another: Vector2D): Vector2D = new Vector2D(this.x - another.x, this.y - another.y)

  def *(multiplier: Float): Vector2D = new Vector2D(multiplier * this.x, multiplier * this.y)

  // dot product of two vectors
  def *(another: Vector2D): Float = this.x * another.x + this.y * another.y

  def unary_- = new Vector2D(-this.x, -this.y)

  def normalize: Vector2D = new Vector2D(this.x / this.magnitude, this.y / this.magnitude)

  def magnitude: Float = sqrt(this.x * this.x + this.y * this.y).toFloat

  def rotatedBy(angle: Float): Vector2D = new Vector2D(
    cos(angle).toFloat * this.x - sin(angle).toFloat * this.y,
    sin(angle).toFloat * this.x + cos(angle).toFloat * this.y)

  // this returns a Float between 0 and Pi
  def angleBetween(another: Vector2D): Float = acos( (this * another) / (this.magnitude * another.magnitude) ).toFloat

  // the angle is between the vector and the x-axis on the x-positive side of the vector, radians
  // needs to be between (both ends inclusive) 0 and 2Pi radians
  // OBS! The GUI-library has the origo on the top left and the y-coordinate increases downwards, x "as normal"
  def angle: Float = {
    var ret: Float = 0f
    if (this.x >= 0 && this.y >= 0) {
      ret = atan(1.0 * this.y / this.x).toFloat
      require(ret >= 0)
      require(ret <= (Pi/2).toFloat)

    } else if (this.x < 0 && this.y >= 0) {
      ret = (Pi - atan(1.0*this.y / (-this.x))).toFloat
      require(ret >= (Pi/2).toFloat)
      require(ret <= Pi.toFloat)

    } else if (this.x < 0 && this.y < 0) {
      ret = (Pi + atan(-1.0*this.y / (-this.x))).toFloat
      require(ret >= Pi.toFloat)
      require(ret <= (3*Pi/2).toFloat)

    } else if (this.x == 0f && this.y == 0f) {
      // do nothing, ret should just be 0 and it was instantiaded with that value already
    } else {
      ret = (2*Pi - atan(-1.0*this.y / this.x)).toFloat
      require(ret >= (3*Pi/2).toFloat)
      require(ret <= (2*Pi).toFloat)
    }

    require(ret <= (2*Pi).toFloat)
    require(ret >= 0)
    ret
  }
}

object Vector2D {
  // unit vectors for the 2d space
  def xUnit: Vector2D = new Vector2D(1f, 0f)
  def yUnit: Vector2D = new Vector2D(0f, 1f)
  def origo: Vector2D = new Vector2D(0f, 0f)

  // create a unit vector (magnitude 1) with angle from the input
  def unitVectorFromAngle(ang: Float): Vector2D = {
    require(ang >= 0)
    require(ang <= 2*Pi)

    var xNeg = 1.0f
    var yNeg = 1.0f

    // I'll just make sure the unit vector has the right direction with these if-sentences.
    // This way I don't have to do some math-thinking for the tangent-function to have the
    // directions correct: I can just use the xNeg and yNeg with the absolute value of the tan
    if (ang >= 0 && ang <= Pi/2) {
      xNeg = 1.0f
      yNeg = 1.0f
    } else if (ang > Pi/2 && ang <= Pi) {
      xNeg = -1.0f
      yNeg = 1.0f
    } else if (ang > Pi && ang <= (3*Pi)/2) {
      xNeg = -1.0f
      yNeg = -1.0f
    } else {
      xNeg = 1.0f
      yNeg = -1.0f
    }

    val ret: Vector2D = xUnit * xNeg + (yUnit * abs(tan(ang)).toFloat * yNeg)
    ret.normalize
  }
}

//class Velocity(x: Float, y: Float) extends Vector2D(x, y)
