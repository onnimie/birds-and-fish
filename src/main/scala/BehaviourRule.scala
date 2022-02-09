import scala.util.Random

abstract class BehaviourRule {
  /**
   * An object that is a BehaviourRule has an only purpose of
   * creating a Vector2D for a Boid's advance()-method.
   * A BehaviourRule also has a power that determines the
   * impactfulness of the rule for a Boid. This is meaningful when
   * there are several rules affecting a Boid
   *
   */

  // UI will have sliders that affect this value
  var power: Int
  val powerMin = 0
  val powerMax = 100

  // for UI-checkbox (toggle effect)
  var notIgnored: Boolean

  // name for UI-elements
  val name: String

  def apply(b: Boid, nearbyBoids: Vector[Boid]): Vector2D


}

object Separation extends BehaviourRule {
  var power = 100
  var notIgnored = true
  val name = "Separation"

  def apply(b: Boid, nearbyBoids: Vector[Boid]): Vector2D = {
    /**
     * The boid should avoid colliding with other boids.
     */

    if (notIgnored) {

      val weightFactor = power / 80f

      var ret: Vector2D = Vector2D.origo
      for (i <- nearbyBoids) {
        // b is the caller boid
        val awayVector = b.pos - i.pos
        ret = ret + (awayVector.normalize * (5f / awayVector.magnitude ) )

      }

      if (ret.x == 0f && ret.y == 0f) {
        // return zero-vector if nobody is nearby
        Vector2D.origo
      } else {
        ret.normalize * weightFactor
      }

    } else {
      // return a zero-vector that has no effect on the boid
      Vector2D.origo
    }
  }
}

object Cohesion extends BehaviourRule {
  var power = 100
  var notIgnored = true
  val name ="Cohesion"

  def apply(b: Boid, nearbyBoids: Vector[Boid]): Vector2D = {
    /**
     * The boid should move towards the center of the flock
     */

    // also take into account whether there are any boids nearby, so that loner-boids dont head towards (0,0)
    if (notIgnored && nearbyBoids.nonEmpty) {

      val weightFactor = power / 90f

      // b is the caller boid
      val targetX = nearbyBoids.foldLeft(0f)( (a, b) => a + (b.pos.x)/nearbyBoids.size )
      val targetY = nearbyBoids.foldLeft(0f)( (a, b) => a + (b.pos.y)/nearbyBoids.size )
      val targetPos = new Vector2D(targetX, targetY)

      // return a vector toward the target position
      (targetPos - b.pos).normalize * weightFactor

    } else {
      // return a zero-vector, in other words: no effect
      Vector2D.origo
    }
  }
}

object Alignment extends BehaviourRule {
  var power = 100
  var notIgnored = true
  val name = "Alignment"

  def apply(b: Boid, nearbyBoids: Vector[Boid]): Vector2D = {
    /**
     * The boid should try to move towards the same direction as its nearby friends,
     * in other words: the boid should try to align its direction with the others.
     */

    if (notIgnored) {

      val sumDirection = nearbyBoids.foldLeft[Vector2D](Vector2D.origo)( (f, g) => {
        // b is the caller boid
        val distance = (g.pos - b.pos).magnitude
        // take into account, that the further the boid, the less impact it should have on the alignment
        f + (g.vel * (5f / distance))
      })


      val weightFactor = power / 35f

      // return a vector that points towards the sumDirection of the nearby boids if there were any
      if (sumDirection.x == 0f && sumDirection.y == 0) {
        Vector2D.origo
      } else {
        (sumDirection).normalize * weightFactor
      }


    } else {
      // no effect if ignored
      Vector2D.origo
    }
  }
}

object Randomization extends BehaviourRule {
  var power = 100
  var notIgnored = true
  val name = "Randomization"

  def apply(b: Boid, nearbyBoids: Vector[Boid]): Vector2D = {
    /**
     * The boid should change its direction randomly ever-so-slightly but every frame
     */

    if (notIgnored) {

      val weightFactor = power * power * 1f

      val rand = new Random(System.nanoTime)
      val randomRotation = (rand.nextGaussian() / 15).toFloat

      // b is the caller boid
      b.vel.rotatedBy(randomRotation) * weightFactor

    } else {
      // no effect if ignored
      Vector2D.origo
    }
  }
}