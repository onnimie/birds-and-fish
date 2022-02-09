import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer

class GridSquare(sideLength: Float, position: Vector2D) {

  /**
   * The idea of this class and the Grid-class is to create a grid onto the simulation window,
   * so that the boids are always inside a GridSquare.
   * This allows hopefully faster calculation for the boid's nearby boids.
   * Before GridSquares, each boid calculated distances and angles between every other boid.
   * With a grid that consists of GridSquares, each as long as the boid's visionDistance,
   * we can just calculate the boid's nearby boids just by using it's nearby grids.
   *
   * The position of GridSquare is its left upper corner (according to users eye)
   */

  private val xBounds: (Float, Float) = (position.x, position.x + sideLength)
  private val yBounds: (Float, Float) = (position.y, position.y + sideLength)

  val posInt: (Int, Int) = (position.x.toInt, position.y.toInt)

  def contains(b: Boid): Boolean = {
    val pos = b.pos
    (pos.x >= xBounds._1 && pos.x <= xBounds._2) && (pos.y >= yBounds._1 && pos.y <= yBounds._2)
  }


}

// this class holds the GridSquares and useful methods and Maps
class Grid(rows: Int, columns: Int, squareSideLength: Float, startPos: Vector2D) {

  // first we list rows, and then columns
  // this list will begin from top left (first row, first column)
  val gridSquares: Vector[Vector[GridSquare]] = {


    val buffer = Buffer[Vector[GridSquare]]()

    var (posX, posY) = (startPos.x, startPos.y)
    for (i <- 0 until rows) {
      val r = Buffer[GridSquare]()

      val ypo = posY + i * squareSideLength

      for (j <- 0 until columns) {

        val xpo = posX + j * squareSideLength
        r.append( new GridSquare(squareSideLength, new Vector2D(xpo, ypo)))
      }


      buffer.append(r.toVector)
    }
    buffer.toVector
  }

  val neighboursMap: HashMap[GridSquare, Vector[GridSquare]] = {

    val map = HashMap[GridSquare, Vector[GridSquare]]()

    for (row <- gridSquares){
      for (gridSquare <- row) {

        // add this one already
        val buffer = Buffer[GridSquare](gridSquare)

        val columnIndex = row.indexOf(gridSquare)
        val rowIndex = gridSquares.indexOf(row)

        // tend to the special cases first
        if (rowIndex == 0) {
          if (columnIndex == 0) {
            buffer.append(gridSquares(rowIndex)(columnIndex + 1))
            buffer.append(gridSquares(rowIndex + 1)(columnIndex))
            buffer.append(gridSquares(rowIndex + 1)(columnIndex + 1))
          } else if (columnIndex == columns - 1){
            buffer.append(gridSquares(rowIndex)(columnIndex - 1))
            buffer.append(gridSquares(rowIndex + 1)(columnIndex))
            buffer.append(gridSquares(rowIndex + 1)(columnIndex - 1))
          } else {
            buffer.append(gridSquares(rowIndex)(columnIndex + 1))
            buffer.append(gridSquares(rowIndex)(columnIndex - 1))
            buffer.append(gridSquares(rowIndex + 1)(columnIndex + 1))
            buffer.append(gridSquares(rowIndex + 1)(columnIndex))
            buffer.append(gridSquares(rowIndex + 1)(columnIndex - 1))
          }
        } else if (rowIndex == rows - 1) {
          if (columnIndex == 0) {
            buffer.append(gridSquares(rowIndex)(columnIndex + 1))
            buffer.append(gridSquares(rowIndex - 1)(columnIndex))
            buffer.append(gridSquares(rowIndex - 1)(columnIndex + 1))
          } else if (columnIndex == columns - 1){
            buffer.append(gridSquares(rowIndex)(columnIndex - 1))
            buffer.append(gridSquares(rowIndex - 1)(columnIndex))
            buffer.append(gridSquares(rowIndex - 1)(columnIndex - 1))
          } else {
            buffer.append(gridSquares(rowIndex)(columnIndex + 1))
            buffer.append(gridSquares(rowIndex)(columnIndex - 1))
            buffer.append(gridSquares(rowIndex - 1)(columnIndex + 1))
            buffer.append(gridSquares(rowIndex - 1)(columnIndex))
            buffer.append(gridSquares(rowIndex - 1)(columnIndex - 1))
          }
        } else if (columnIndex == 0) {
          // we already checked the corners above
          buffer.append(gridSquares(rowIndex)(columnIndex + 1))
          buffer.append(gridSquares(rowIndex + 1)(columnIndex))
          buffer.append(gridSquares(rowIndex + 1)(columnIndex + 1))
          buffer.append(gridSquares(rowIndex - 1)(columnIndex + 1))
          buffer.append(gridSquares(rowIndex - 1)(columnIndex))
        } else if (columnIndex == columns - 1) {
          // again, we have already checked the corners
          buffer.append(gridSquares(rowIndex - 1)(columnIndex - 1))
          buffer.append(gridSquares(rowIndex - 1)(columnIndex))
          buffer.append(gridSquares(rowIndex)(columnIndex - 1))
          buffer.append(gridSquares(rowIndex + 1)(columnIndex - 1))
          buffer.append(gridSquares(rowIndex + 1)(columnIndex))
        } else {
          // if the check has gone this far, this gridSquare has all possible 8 neighbours existing
          buffer.append(gridSquares(rowIndex - 1)(columnIndex - 1))
          buffer.append(gridSquares(rowIndex - 1)(columnIndex))
          buffer.append(gridSquares(rowIndex - 1)(columnIndex + 1))
          buffer.append(gridSquares(rowIndex)(columnIndex - 1))
          buffer.append(gridSquares(rowIndex)(columnIndex + 1))
          buffer.append(gridSquares(rowIndex + 1)(columnIndex - 1))
          buffer.append(gridSquares(rowIndex + 1)(columnIndex))
          buffer.append(gridSquares(rowIndex + 1)(columnIndex + 1))
        }

        map += (gridSquare -> buffer.toVector)
      }
    }

    map
  }

  // this is empty at first, the Simulation should update this grid
  val boidsMap: HashMap[GridSquare, Buffer[Boid]] = {
    val map = HashMap[GridSquare, Buffer[Boid]]( (null, Buffer[Boid]()) )
    for (gridSquare <- neighboursMap.keys) {
      map += (gridSquare -> Buffer[Boid]())
    }
    map
  }

  val squares: Vector[GridSquare] = neighboursMap.keys.toVector

  // the Simulation should call this method every time it advances
  def updateBoidsMap(sim: Simulation) = {
    for (square <- squares) {
      for (boid <- sim.boidList) {
        val boidsCurrentGrid = boid.gridSquare
        if ((boidsCurrentGrid != square) && square.contains(boid)) {
          // remove info of the old position in grid
          boidsMap(boidsCurrentGrid) -= boid
          // update info in the gridmap
          boidsMap(square).append(boid)
          // update for boid
          boid.gridSquare = square
        }
      }
    }
  }

  // clear every boid from the map
  def clearBoidsMap() = {
    for (square <- squares) {
      boidsMap(square).clear()
    }
  }

  // removes the given boid from the boidsMap
  def removeBoidFromGrid(b: Boid) = {

    for (square <- squares) {
      if (square.contains(b)){
        boidsMap(square) -= b
      }
    }
  }


}
