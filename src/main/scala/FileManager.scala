import java.io.FileReader
import java.io.BufferedReader
import java.io.FileNotFoundException
import java.io.IOException
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Calendar

object FileManager {
  /**
   * This object's sole purpose in life is to create
   * an instance of Simulation from a file.
   * The UI-object will use this object to create a Simulation.
   *
   * The fileformat is just simple rows of text with indicator prefixes.
   * Each row's type of information is determined by the prefix (3 characters).
   * If an information piece is missing, a default value is used.
   * Whitespace is allowed anywhere, but each information piece needs to be on a line of its own.
   *
   * There is an option for saving a scene, which will create a new file
   * that has the exact information of a simulation at the frame of pressing the save button.
   *
   * Information types:
   * POP --- population, ex. POP 250
   * RUL --- behaviorRule information (name, power, enabled), ex. RUL Cohesion ;88 ;1
   * SPE --- the simulation's speed (boids velocity in Simulation), ex. SPE 0.5
   * BOI --- a single boid info (pos, vel) , ex. BOI 124.54351,581.12424;12.415,16.12
   *
   * Thus, if a save file is created, it will mostly consist of lines of boid-data.
   * Notice, that an input file doesn't have to list each boid of the pop:
   * the default values of BOI are randomized.
   */


  // The UI-object calls this method with a path given by the user
  def loadScene(sourcePath: String): Simulation = {
    val simulation = new Simulation

    // get Options for the Readers so that they can be assessed in the finally-bracket
    var lineInOption: Option[BufferedReader] = None
    var fileInOption: Option[FileReader] = None

    try {
      // read file, get BufferedReader (each line as string)
      val fileIn = new FileReader(sourcePath)
      val lineIn = new BufferedReader(fileIn)

      // get these readers into variables outside the try-bracket
      lineInOption = Some(lineIn)
      fileInOption = Some(fileIn)

      // work through each line from the file
      var popCount = 0 // this will increase from POP, decrease from BOI

      var oneLine: String = null   // oneLine will have the value of each line of the file
      while ({oneLine = lineIn.readLine(); oneLine != null}) {

        val prefix: String = oneLine.trim.take(3)
        val suffix: String = oneLine.trim.drop(3)

        prefix match {
          case "POP" => popCount = popCount + suffix.trim.toInt

          case "RUL" => {

            // get the info components from the line (suffix)
            val infoComponents = suffix.split(';')

            // separate the info components to individual variables
            val ruleName: String = infoComponents(0).trim
            val power: Int = infoComponents(1).trim.toInt
            val enabled: Int = infoComponents(2).trim.toInt

            for (rule <- simulation.rules){
              // check which rule is this infopiece about
              if (ruleName == rule.name){
                // get the power value
                if (power <= 100 && power >= 0) {
                  rule.power = power
                } else { // give 0 weight if power is given a value outside the range between 0 and 100
                  rule.power = 0
                }
                // get notIgnored boolean
                if (enabled == 1) {
                  rule.notIgnored = true
                } else {
                  rule.notIgnored = false
                }
              }
            }
          }

          case "SPE" => {
            // get the value from suffix
            val value: Float = suffix.trim.toFloat

            // check if the given value is outside the range of 0.1-20.0
            if (value < 0.1f) {
              simulation.boidVelocity = 0.1f
            } else if (value > 20.0) {
              simulation.boidVelocity = 20.0f
            } else {
              simulation.boidVelocity = value
            }
          }

          case "BOI" => {
            // format is BOI posX,posY;velX,velY (these are floats)
            // ex. BOI 12.1245,16.17 ; 127.134,1.2
            val stringxy: Array[String] = suffix.trim.takeWhile((c: Char) => c != ';').trim.split(',')
            val posxy: Array[Float] = stringxy.map(_.toFloat)

            val stringvel: Array[String] = suffix.trim.dropWhile((c: Char) => c != ';').drop(1).trim.split(',')
            val velxy: Array[Float] = stringvel.map(_.toFloat)

            val boid = new Boid(new Vector2D(posxy(0), posxy(1)), new Vector2D(velxy(0), velxy(1)))
            simulation.addBoid(boid)

            // keep count of remaining, to-be-created population
            popCount -= 1
          }
          case _ => // do nothing
        }
      }

      // popCount should be above 0 if the amount of BOI-lines didn't reach
      // the amount indicated by the POP-lines
      // popCount is negative when there are more BOI-lines than what the POP-lines indicate.
      // ^this is okay, we don't have to worry about that
      if (popCount > 0) {
        for (i <- 0 until popCount){
          simulation.addRandomBoid()
        }
      }

    } catch {
          // throw a dialog to the user informing them something went wrong
      case e: FileNotFoundException => Application.errorFileNotFound()
      case f: IOException => Application.errorIO()
      case g: NumberFormatException => Application.errorNumberFormat()
      case _: Throwable => Application.errorUnspecified()

    } finally {
      // close the streams
      if (lineInOption.nonEmpty) lineInOption.get.close()
      if (fileInOption.nonEmpty) fileInOption.get.close()
    }

    simulation
  }




  // this method saves the current ongoing simulation, each boid and everything, onto a txt-file
  // into the user-given directory
  def saveScene(directory: String, fileName: String, simulation: Simulation) = {

    // get writers outside the try, so that they can be closed in finally-section
    var fileOutOption: Option[FileWriter] = None
    var lineOutOption: Option[BufferedWriter] = None

    try {
      // get the writers
      val fileOut = new FileWriter(directory + "\\" + fileName + ".txt")
      val lineOut = new BufferedWriter(fileOut)

      // if getting writers was successful, store them inside the option-containers
      fileOutOption = Some(fileOut)
      lineOutOption = Some(lineOut)

      // get meta info
      val time = Calendar.getInstance()
      val meta = "Saved scene from Boids-simulation on " + time.getTime() + "\n------------------------------\n"
      // write metainfo
      lineOut.write(meta + "\n")

      // write population info
      lineOut.write("\nPOP " + simulation.population)

      // write rule info for each rule
      simulation.rules.foreach( (r: BehaviourRule) => lineOut.write("\nRUL " + r.name + "; " + r.power + "; " + (if (r.notIgnored) 1 else 0)) )

      // write boid velocity / sim speed info
      lineOut.write("\nSPE " + simulation.boidVelocity)

      // write boids
      simulation.boidList.foreach( (b: Boid) => lineOut.write("\nBOI " + b.pos.x + "," + b.pos.y + ";" + b.vel.x + "," + b.vel.y) )

    } catch {
          // throw error messages to the user
      case e: FileNotFoundException => Application.errorFileNotFound()
      case f: IOException => Application.errorIO()
      case g: Throwable => Application.errorUnspecified()

    } finally {
      // finally close the streams
      if (lineOutOption.nonEmpty) lineOutOption.get.close()
      if (fileOutOption.nonEmpty) fileOutOption.get.close()
    }
  }
}
