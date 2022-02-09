import scala.swing._
import scala.math.round
import scala.math.sin
import scala.math.Pi
import scala.util.Random
import java.awt.Color
import java.awt.Polygon
import java.awt.Rectangle
import java.awt.event.ActionListener
import java.awt.Font
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

object Application extends SimpleSwingApplication {

  // dimensions of the MainFrame (the whole app-window)
  private val mainXdim = 1200
  private val mainYdim = 650

  // this is used for the window size of the simulation
  // also, Boid-class will use these bounds in its advance()-method and Simulation for its defaultSim()
  val simWinXbound = (7, 1170)
  val simWinYbound = (7, 440)

  // The sides next to the simWindows are filled with black,
  // the height of the black bar under the simWindow is defined here
  private val simWinUnderbarHeight = 7

  // fill in a bit of space after the underbar
  private val fillerAfterUnderBarHeight = 7

  // The sizes for the two components inside the single BoxPanel-container, so they sit nicely
  private val simAreaSize = new Dimension(mainXdim, simWinYbound._1 + simWinYbound._2 + simWinUnderbarHeight)
  private val controlAreaSize = new Dimension(mainXdim, mainYdim - (simWinYbound._1 + simWinYbound._2 + simWinUnderbarHeight) - 50)

  // this value is used to determine the sizes of the sliders in the rulePanel (right side of control panel)
  private val ruleSliderSize = new Dimension(130, 20)

  // give value for the size of the boid velocity -slider
  private val boidVelocitySliderSize = new Dimension(100, 20)

  // set the side length of a boid, which is drawn as an even triangle
  private val boidSideLength = 7

  private val fileManagerNameInDialog = "File Management"

  // set the default simulation up
  private var simulation = FileManager.loadScene(".\\defaultSim.txt")

  // set up a grid
  val grid = new Grid(15, 40, Simulation.boidVisionDistance, Vector2D.origo)
  simulation.assignBoidsGridSquares(grid)

  // this value determines whether the grid and boids' vision circles are drawn onto the simulation window
  private var showTheMatrix = false

  private val helpString: String = {

   "Simulation Controls: \n" +
   "play/pause --- press this button first to start the simulation and again to pause it.\n" +
   "respawn randomly --- this button randomises a new position for every boid.\n" +
   "speed --- change the slider to speed up or slow down the simulation.\n" +
   "add boid --- add a new boid with a random position and rotation to the simulation.\n" +
   "remove boid --- remove a random boid from the simulation.\n\n" +
    // new paragraph
   "Change up the behaviour of the boids!\n" +
   "Look into the documentation for further information on each rule and how they affect a boid.\n" +
   "Slide the sliders to vary the impactfulness of each rule\n" +
   "or disable them completely by unchecking the checkboxes!\n\n" +
    // new paragraph
   "Create your own simulation startup settings!\n" +
   "You can save existing settings into a txt-file by writing the save-directory's path\n" +
   "to the text field where it says 'enter path to a save directory' and press 'save'\n" +
   "OR you can create your own from scratch!\n" +
   "Take a further look into the defaultSim.txt-file on how to format the information\n" +
   "If you've a load-file, just enter its full path to the text field above the load-button\n" +
   "and press 'load'.\n\n"

  }

  /**
   * Here are defined all the panels and components for the MainFrame-window.
   * First we define the simulation window where the boids fly free (simulationArea)
   *
   * Then right after we start forming the control panel where all the controls are.
   * First with the BehaviourRule-controls that will be on the right.
   * Then general simulation controls that are on the middle-right.
   *
   */


  private val simulationArea = new Panel {
    override def paintComponent(g: Graphics2D) = {

      // clear the area, paint over the previous boids
      g.setColor(Color.gray)
      g.fillRect(simWinXbound._1, simWinYbound._1, simWinXbound._2, simWinYbound._2)

      // draw the boids
      g.setColor(Color.white)

      simulation.boidList.foreach( (b: Boid) => drawBoid(b.pos, b.angle, g) )


      // frame the simWin with black
      g.setColor(Color.black)

      g.fillRect(0, 0, simWinXbound._1, simWinYbound._2 + simWinYbound._1)
      g.fillRect(simWinXbound._2 + simWinXbound._1, 0, mainXdim, simWinYbound._2 + simWinYbound._1)
      g.fillRect(simWinXbound._1, 0, simWinXbound._2, simWinYbound._1)
      g.fillRect(0, simWinYbound._2, mainXdim, simWinUnderbarHeight)

      // fill in some background color after the black underbar
      g.setColor(controlArea.background)
      g.fillRect(0, simWinYbound._2 + simWinUnderbarHeight, mainXdim, fillerAfterUnderBarHeight)

      // show the grid and boid vision circles
      if (showTheMatrix) {

        g.setColor(Color.red)
        val distance = Simulation.boidVisionDistance.toInt
        for (boid <- simulation.boidList) {
          g.drawOval(boid.pos.x.toInt - distance, boid.pos.y.toInt - distance, distance * 2, distance * 2)
        }

        g.setColor(Color.cyan)
        for (square <- grid.squares){
          g.drawRect(square.posInt._1, square.posInt._2, 30, 30)
        }
      }

    }

    preferredSize = simAreaSize
    minimumSize = simAreaSize
    maximumSize = simAreaSize
  }

  /**
   * BehaviourRule-controls from here on.
   */

  // use this function to create a flowpanel container for a rule-controller
  // this function also returns the controller components so that they can be assinged to variables
  private def makeRulePanelSliderCheckBoxLabel(rule: BehaviourRule): (Panel, Slider, CheckBox, Label) = {
    val nimi = rule.name
    val power = rule.power
    val enabled = rule.notIgnored
    val startingValue = rule.power

    val nameLabel = new Label(nimi) // just the name of the rule

    // return this label so that an event handler can update its text
    val weightLabel = new Label(startingValue.toString)

    // return this slider so that an event handler can access it through a variable
    val slider = new Slider {
      orientation = Orientation.Horizontal
      value = startingValue
      min = rule.powerMin
      max = rule.powerMax
      // size determined under the Application-objects definition, where other values are given
      minimumSize = ruleSliderSize
      maximumSize = ruleSliderSize
      preferredSize = ruleSliderSize
    }

    // return the checkbox so that it can be assigned to a reference variable that the event handler can handle
    val checkbox = new CheckBox {
      selected = true
    }

    val boxPanel = new BoxPanel(Orientation.Horizontal) {
      contents += nameLabel
      contents += slider
      contents += weightLabel
      contents += checkbox
    }
    val flowPanel = new FlowPanel(nameLabel, slider, weightLabel, checkbox)

    // we can choose between the box layout or the flow layout
    (flowPanel, slider, checkbox, weightLabel)
  }

  // get a rulepanel (the function above) for each rule listed in the simulation's rules
  private val rulePanelTuples: Vector[(Panel, Slider, CheckBox, Label)] = simulation.rules.map(makeRulePanelSliderCheckBoxLabel(_))

  private val rulePanel = new BoxPanel(Orientation.Vertical) {
    contents += new Label("Change the weight of each rule") {
      font = new Font("Ariel", Font.ITALIC, 11)
    }

    for (rulePanelTuple <- rulePanelTuples) {
      contents += rulePanelTuple._1
    }
  }

  /**
   * General simulation controls from here on.
   */

  private val boidVelocitySlider = new Slider {
    orientation = Orientation.Horizontal
    value = (simulation.boidVelocity * 10).toInt
    min = 1
    max = (simulation.maxBoidVelocity * 10).toInt

    // get size from way above (where several other variables are also given values)
    minimumSize = boidVelocitySliderSize
    maximumSize = boidVelocitySliderSize
    preferredSize = boidVelocitySliderSize
  }

  private val boidVelocitySliderValueLabel = new Label(simulation.boidVelocity.toString)

  private val pauseButton = new Button("pause")
  private val randomRespawnButton = new Button("respawn randomly")

  private def switchPauseText() = {
    if (pauseButton.text == "pause") {
      pauseButton.text = "play"
    } else {
      pauseButton.text = "pause"
    }
  }

  private val addBoidButton = new Button("add boid")
  private val removeBoidButton = new Button("remove boid")

  private val populationLabel = new Label("population:    " + simulation.population) {
    font = new Font("Ariel", Font.PLAIN, 10)
  }

  // this panel should include some simulation controls such as pause/resume etc.
  private val simButtonsPanel = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel(new Label("Simulation controls"))
    contents += new FlowPanel(pauseButton ,randomRespawnButton)
    contents += new FlowPanel(new Label("speed"), boidVelocitySlider, boidVelocitySliderValueLabel)
    contents += new FlowPanel(addBoidButton, removeBoidButton)
    contents += (populationLabel)
  }

  /**
   * For the panel in the middle right, we have a simple matrix button and a help button.
   */
  val showMatrixButton = new Button("matrix")
  val helpButton = new Button("help")

  private val debugPanel = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel(new Label("Cool buttons"))
    contents += new FlowPanel()
    contents += new FlowPanel(helpButton, showMatrixButton)
    contents += new FlowPanel()

  }

  /**
   * File management controls from here on.
   */

  private val loadTextField = new TextField("enter path to a load file")
  private val loadButton = new Button("load")
  private val saveTextField = new TextField("enter path to a save directory")
  private val saveButton = new Button("save")

  // this panel should contain the buttons for loading and saving and a text field for paths for both
  private val fileManagerPanel = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel(new Label("Load or save simulation scenes"))
    contents += new Label("")
    contents += loadTextField
    contents += new FlowPanel(new Label("^ load a scene from this filepath: "), loadButton)
    contents += saveTextField
    contents += new FlowPanel(new Label("^ save a scene to this directory: "), saveButton)
  }

  /**
   * And lastly, we gather all the control panels to the control area.
   * Then we add the simulation area and the control area to a containing panel,
   * and define the MainFrame (top-function)
   */

  // this will be the area in which all the buttons and other interactivities lay
  private val controlArea = new GridPanel(1, 4) {

    contents += fileManagerPanel
    contents += debugPanel
    contents += simButtonsPanel
    contents += rulePanel

    preferredSize = controlAreaSize
    minimumSize = controlAreaSize
    maximumSize = controlAreaSize

    override def background = Color.lightGray
  }

  private val wholeThing = new BoxPanel(Orientation.Vertical) {
    contents += simulationArea
    contents += controlArea
  }

  def top = new MainFrame {
    title = "Back to school, flock of boids!"
    contents = wholeThing
    size = new Dimension(mainXdim,mainYdim)
    resizable = false

    // the timer (or something else) doesn't seem to quit without this
    // the application seemingly continues to run when MainFrame is closed without this
    override def closeOperation() = {
      timer.stop()
      Application.quit()
    }
  }

  /**
   * Here is defined with the drawBoid-function how the boids should be drawn
   * based on their position and rotation
   */


  // Boid's position should be the center of an even triangle
  private def drawBoid(pos: Vector2D, rotation: Float, g: Graphics2D) = {
    // side-length of the triangle about 7 pix?
    val side = boidSideLength
    // b is the distance between a corner and the center
    val b = (( sin(Pi/6)/sin(2*Pi/3) ) * side).toFloat

    val noseVector = Vector2D.unitVectorFromAngle(rotation) * b

    val rightWingRot = if (rotation - 2*Pi/3 < 0) (rotation + 4*Pi/3).toFloat else (rotation - 2*Pi/3).toFloat
    val leftWingRot = if (rotation + 2*Pi/3 > 2*Pi) (rotation - 4*Pi/3).toFloat else (rotation + 2*Pi/3).toFloat

    val rightWingVector = Vector2D.unitVectorFromAngle(rightWingRot) * b
    val leftWingVector = Vector2D.unitVectorFromAngle(leftWingRot) * b

    val xPos_nose_right_left: Array[Int] = Array(
      round(pos.x + noseVector.x),
      round(pos.x + rightWingVector.x),
      round(pos.x + leftWingVector.x))

    val yPos_nose_right_left: Array[Int] = Array(
      round(pos.y + noseVector.y),
      round(pos.y + rightWingVector.y),
      round(pos.y + leftWingVector.y))

    g.fill( new Polygon(xPos_nose_right_left, yPos_nose_right_left, 3) )
  }


  /**
   * Here lay the event listening and handling
   */

  // Firstly, we define some functions that handle updating the GUI-elements' values

  def updatePop() = populationLabel.text = "population:    " + simulation.population
  def updateSpeed() = {
    boidVelocitySlider.value = (simulation.boidVelocity * 10).toInt
    boidVelocitySliderValueLabel.text = simulation.boidVelocity.toString
  }
  def updateRules() = {
    // indices match between simulation rules and rulePanelTuples
    for (i <- simulation.rules.indices) {
      rulePanelTuples(i)._2.value = simulation.rules(i).power // update slider
      rulePanelTuples(i)._3.selected = simulation.rules(i).notIgnored // update checkbox
    }
  }


  //listen to all the rule controls (slider and checkbox)
  rulePanelTuples.foreach( tuple => {
    listenTo(tuple._2)
    listenTo(tuple._3)
  }) // rulePanelTuple is of (Panel, Slider, CheckBox, Label)

  listenTo(boidVelocitySlider)
  listenTo(pauseButton)
  listenTo(randomRespawnButton)
  listenTo(addBoidButton)
  listenTo(removeBoidButton)

  listenTo(showMatrixButton)
  listenTo(helpButton)

  listenTo(loadButton)
  listenTo(saveButton)

  // gather the pause-methods to one single function
  private def togglePause() = {
    switchPauseText()
    simulation.togglePause()
  }

  // switch matrix from matrix button
  private def toggleMatrix() = showTheMatrix = !showTheMatrix

  this.reactions += {
    case scala.swing.event.ValueChanged(source) => {
      val slider = source.asInstanceOf[Slider]

      // check each rule
      for (i <- rulePanelTuples.indices){
        // each rule has the same index also in simulation.rules, because the ruleTuples is mapped from that
        if (slider == rulePanelTuples(i)._2) {
          simulation.rules(i).power = slider.value
          rulePanelTuples(i)._4.text = slider.value.toString
        }
      }

      // check if speed-slider
      if (slider == boidVelocitySlider){
        simulation.boidVelocity = slider.value / (10f)
        boidVelocitySliderValueLabel.text = simulation.boidVelocity.toString
      }
    }

    case scala.swing.event.ButtonClicked(source) => {
      // source is of type AbstractButton (supertype of the buttons and checkboxes)

      source match {
        // check for each checkbox in the rule panel
        case checkbox: CheckBox => {

          for (i <- rulePanelTuples.indices) {
            // each rulePanelTuple has the same index in this vector than
            // its corresponding rule in the rules-list in Simulation
            if (checkbox == rulePanelTuples(i)._3) {
              simulation.rules(i).notIgnored = checkbox.selected
            }
          }
        }
        // check for other buttons
        case button: Button => {

          // react to pause-event
          if (button == pauseButton) {
            togglePause()
          }
          // react to random respawn -button click
          if (button == randomRespawnButton) {
            simulation.randomRespawn(grid)
          }
          // react to add boid -button pressed
          if (button == addBoidButton) {
            simulation.addRandomBoid(grid)
            updatePop()
          }
          // react to remove boid -button pressed
          if (button == removeBoidButton) {
            if (simulation.boidList.nonEmpty) {
              simulation.removeBoid(simulation.boidList.head, grid)
              updatePop()
            }
          }
          // react to load button pressed
          if (button == loadButton) {


            // get load file path from textfield
            val path = loadTextField.text
            // change the simulation-variable to hold the new simulation from the load file
            simulation = FileManager.loadScene(path)

            // update grid stuff
            grid.clearBoidsMap()
            // FileManager doesn't assign the boids to any grid
            simulation.assignBoidsGridSquares(grid)

            // the simulation is not paused by default
            simulation.togglePause()

            // make sure the pause-button says "play"
            if (pauseButton.text == "pause") switchPauseText()

            // Update the UI-controls to match the simulation data

            updatePop()
            updateRules()
            updateSpeed()

          }
          // react to save button pressed
          if (button == saveButton) {
            // firstly, pause the simulation
            togglePause()

            // get save directory from textfield
            val path = saveTextField.text
            // ask the user for a filename
            val fileName = Dialog.showInput[String](wholeThing, "File Management, hello. Please,\n name the newborn here", fileManagerNameInDialog, Dialog.Message.Question, Swing.EmptyIcon, Nil, "here")
            if (fileName.nonEmpty) {
              // save scene if input was given
              FileManager.saveScene(path, fileName.get, simulation)
            }

            // finally, unpause the simulation
            togglePause()
          }

          if (button == showMatrixButton) {
            toggleMatrix()
          }
          if (button == helpButton) {
            Dialog.showMessage(wholeThing, helpString, "a Guide")
          }
        }
      }


    }
  }


  // ActionListener that calls its actionPerformed each time the timer sends an ActionEvent
  val listener = new ActionListener(){
    def actionPerformed(e: java.awt.event.ActionEvent) = {

      // Update the simulation on its own thread so that the user can interact with
      // the GUI without lag when the simulation is heavy
      Future {
        simulation.advance(grid)
      }.onComplete {
        case r: Success[_] => {
          Swing.onEDT {
            simulationArea.repaint()
          }
        }
        case _ => {
          // skip the advancing of simulation, i guess
          Swing.onEDT {
            simulationArea.repaint()
          }
        }
      }

    }
  }

  // timer that sends ActionEvents to ActionListener (right above)
  // sends the signal every 10 ms
  val timer = new javax.swing.Timer(10, listener)
  togglePause() // pause the simulation at start
  timer.start() // start the flow of time

  /**
   * Here we define some functions for error messages.
   * The FileManager will use these to communicate errors to the user through Application
   */

  def errorFileNotFound() = Dialog.showMessage(wholeThing, "We couldn't find this filepath!", fileManagerNameInDialog)
  def errorIO() = Dialog.showMessage(wholeThing, "Something went wrong... (IO exception)", fileManagerNameInDialog)
  def errorNumberFormat() = Dialog.showMessage(wholeThing, "Where there are supposed to be numbers in your file..\nWell, we found letters.", fileManagerNameInDialog)
  def errorUnspecified() = Dialog.showMessage(wholeThing, "Something went wrong... (unspecified)", fileManagerNameInDialog)
}
