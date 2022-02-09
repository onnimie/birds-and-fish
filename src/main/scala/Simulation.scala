import Simulation.getRandomPosAndVelWithinSimBounds

import scala.collection.mutable.Buffer
import scala.util.Random
import scala.math.Pi

class Simulation {
  /**
   * This class represents the entire simulation
   * that consists of boids and rules.
   * There should only be a one intance of simulation present
   * at a certain time. Instances 'exist' through the UI.
   */

  // may change the collection datatype to something more efficient
  private val boids: Buffer[Boid] = Buffer[Boid]()

  // maybe this could work as a connection between the BehaviourRules and UI-element?
  // rules are indexed with the same order as they are introduced in the BehaviourRule.scala-file
  val rules: Vector[BehaviourRule] = Vector(Separation, Cohesion, Alignment, Randomization)

  // same velocity for all the boids, UI has access directly to this variable
  var boidVelocity: Float = 2f
  val maxBoidVelocity: Float = 20f

  // The simulation should to able to pause its development
  private var isPaused: Boolean = false

  // access for UI
  def boidList = this.boids.toVector
  def population = this.boids.size

  // The UI can pause/play the simulation
  def togglePause() = this.isPaused = !this.isPaused

  // adds a boid to the list of the boids, gives a value for the boid's simulation-var
  def addBoid(boid: Boid, grid: Grid = null) = {
    this.boids.append(boid)
    boid.addToSim(this)

    if (grid != null) {
      assignBoidGridSquare(boid, grid)
    }
  }

  // adds a boid with a random location and rotation (velocity)
  def addRandomBoid(grid: Grid = null) = {
    val rand = new Random(System.nanoTime)
    // get random init values for boid
    val (pos, vel) = getRandomPosAndVelWithinSimBounds(rand)
    val newBoid = new Boid(pos, vel)

    addBoid(newBoid, grid)
  }

  // removes the boid from the simulation
  def removeBoid(boid: Boid, grid: Grid = null) = {
    this.boids -= boid

    if (grid != null) {
      grid.removeBoidFromGrid(boid)
    }
  }

  // assign the given boid a grid square based on the boids position
  def assignBoidGridSquare(b: Boid, g: Grid) = {
    for (square <- g.squares) {
      if (square.contains(b)) {
        b.gridSquare = square
      }
    }
  }

  // assigns every boid a gridSquare, this is called only once per simulation
  def assignBoidsGridSquares(g: Grid) = {
    for (boid <- boids) {
      assignBoidGridSquare(boid, g)
    }
  }

  // advances all the boids that are listed in the boidList
  def advance(g: Grid = null) = if (!isPaused) {
    this.boids.foreach(_.advance())

    // update grid if one was given
    if (g != null) {
      g.updateBoidsMap(this)
    }
  }

  // respawn every boid with a random position and rotation, return a new Simulation
  def randomRespawn(grid: Grid = null) = {
    val rand = new Random(System.nanoTime)
    for (boid <- this.boids) {
      // remove a boid for each new one (replace old with new ones)
      removeBoid(boid, grid)

      // get random values for the new boid
      val (pos, vel) = Simulation.getRandomPosAndVelWithinSimBounds(rand)
      val newBoid = new Boid(pos, vel)

      addBoid(newBoid, grid)
    }
  }
}

// this object is just used for the initial default simulation
object Simulation {

  val boidVisionDistance: Float = 30f

  def getRandomPosAndVelWithinSimBounds(r: Random): (Vector2D, Vector2D) = {

    val offset = 10f // offset from the actual bounds
    val xBounds = Application.simWinXbound
    val yBounds = Application.simWinYbound

    // get random position within bounds, take offset into account
    val pos = new Vector2D(r.between(xBounds._1 + offset, xBounds._2 - offset), r.between(yBounds._1 + offset, yBounds._2 - offset))

    // get random angle for velocity
    val angle = r.between(0f, (2*Pi).toFloat)
    val velovecto = Vector2D.unitVectorFromAngle(angle)
    val velocity = new Vector2D(velovecto.x, velovecto.y)

    (pos, velocity)
  }

  def defaultSim(): Simulation = {

     // here we create the default simulation that will run when the app is opened
     var simulation = new Simulation
     val rand = new Random(System.nanoTime)
     for (i <- 1 to 100){

       // randomise each boids position and rotation (velocity)
       val (pos, velocity) = getRandomPosAndVelWithinSimBounds(rand)

       simulation.addBoid( new Boid(pos, velocity) )
     }
    simulation
  }
}
