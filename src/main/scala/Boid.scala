class Boid(initPos: Vector2D, initVel: Vector2D) {
  /**
   * A Boid represents a single bird-like entity.
   * A Boid has direction, speed, position, rotation and
   * an advance()-method that changes these.
   */

  private var velocity: Vector2D = initVel
  private var position: Vector2D = initPos
  private var rotation: Float = velocity.angle

  // this variable holds the nearby boids that are visible to this one
  // this variable is always updated every frame, so we can instantiante it with null for now
  // the behaviourRules use this. This value is held by the boid so that the rules don't have to calculate it separately
  private var nearbyBoids: Vector[Boid] = null

  // access to the simulation (access to list of other boids)
  // this variable is assinged its value when a Simulations addBoid-method is called.
  // Boids are always created straight into a simulation, thus a null-value is pretty safe to use
  private var simulation: Simulation = null

  // the simulation should assign each boid their square before the simulation starts running
  // this variable is used in Grid in updateBoidsMap()
  var gridSquare: GridSquare = null

  // values for the vision of the boid
  private val visionDistance: Float = Simulation.boidVisionDistance
  private val visionAngleOneSide: Float = 1.92f

  def addToSim(s: Simulation) = this.simulation = s
  def sim: Simulation = this.simulation

  // access to the parameters of the boid
  def pos = this.position
  def angle = this.rotation
  def vel = this.velocity

  // this method should use all of the BehaviourRules.
  // The BehaviourRules use the calling boid (this) as input.
  def advance(): Unit = {

    // update nearby boids -list every frame
    nearbyBoids = getNearbyBoids(visionDistance, visionAngleOneSide)

    // add the rules to the boids velocity
    for (rule <- this.simulation.rules) {
      this.velocity = this.velocity + rule(this, nearbyBoids)
    }
    // normalize velocity
    this.velocity = this.velocity.normalize * this.simulation.boidVelocity


    val newXpos = this.velocity.x + this.position.x
    val newYpos = this.velocity.y + this.position.y

    this.position = new Vector2D(newXpos, newYpos)
    fixIfOutOfBounds()

    // update rotation if it is calculatable (sometimes the velocity gets the values NaN, no idea where from)
    if (!(this.velocity.x == 0f && this.velocity.y == 0f) && !(this.velocity.x.toString == "NaN" || this.velocity.y.toString == "NaN")) updateRotation()
  }

  // if the boid moves out of bounds, it is teleported to the other side
  private def fixIfOutOfBounds() = {
    val xBounds = Application.simWinXbound
    val yBounds = Application.simWinYbound
    var newXpos = this.pos.x
    var newYpos = this.pos.y
    val offset = 0.1f // offset from the bound just to be sure nobody gets stuck in an endless teleportation loop

    if (this.pos.x <= xBounds._1) {
      newXpos = xBounds._2 - offset
    } else if (this.pos.x >= xBounds._2) {
      newXpos = xBounds._1 + offset
    }
    if (this.pos.y <= yBounds._1) {
      newYpos = yBounds._2 - offset
    } else if (this.pos.y >= yBounds._2) {
      newYpos = yBounds._1 + offset
    }

    this.position = new Vector2D(newXpos, newYpos)
  }

  private def updateRotation() = this.rotation = this.velocity.angle


  // this is used with the function below this one
  private def checkIfNearby(distance: Float, visionAngle: Float, bThat: Boid): Boolean = {
    val distanceVector = bThat.pos - this.pos
    val okDistance = distanceVector.magnitude < distance

    val okVisible = this.vel.angleBetween(distanceVector) < visionAngle

    okVisible && okDistance
  }

  // each boid should be aware of the boids nearby, this function takes parameters for the following:
  // max distance to the boids that are visible to this boids. In other words, the radius of the vision circle
  // vision angle from the front the the side, this one i sused to define the blind angle behind the boid
  // the blind angle behind the boid is 2*Pi-2*visionAngleOneSide
  private def getNearbyBoids(distance: Float, visionAngleOneSide: Float): Vector[Boid] = {
    val allBoids = Application.grid.boidsMap(this.gridSquare).toVector // was sim.boidList before the whole grid-thing happened
    allBoids.filter(checkIfNearby(distance, visionAngleOneSide, _))
  }
}
