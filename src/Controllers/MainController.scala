package com.jamesrthompson.Controllers

import com.jamesrthompson.CLEAN.Cleaner
import com.jamesrthompson.Spots.Spot
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, FXMLLoader, Initializable, JavaFXBuilderFactory}
import javafx.scene.{Group, Scene}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{VBox, AnchorPane}
import javafx.scene.paint.Color
import javafx.scene.shape._
import javafx.scene.control._
import java.io.File
import java.net.URL
import java.util.{ArrayList, ResourceBundle}
import com.jamesrthompson.IO.{ImageLoad, ImageUtils}
import javafx.stage.{Stage, FileChooser}
import javafx.concurrent.{Worker, Task, Service}
import javafx.animation.FadeTransition
import javafx.util.Duration
import jfxtras.labs.scene.control.BigDecimalField
import java.text.{DecimalFormat, NumberFormat}
import java.math.BigDecimal
import java.lang.Boolean

/**
 * Single Molecule Tracking Application Main Controller Class
 * Author: James R. Thompson, D.Phil
 * Date: 7/16/12
 * Time: 9:31 AM
 * USC Mork Family Dept. of Chem. Eng. & Mat. Sci.
 */

class MainController extends Initializable {

  @FXML private[Controllers] var imageView: ImageView = null
  @FXML private[Controllers] var frameSlider: Slider = null
  @FXML private[Controllers] var anchorPane: AnchorPane = null
  @FXML private[Controllers] var progressBar: ProgressBar = null
  @FXML private[Controllers] var leftVBox: VBox = null
  @FXML private[Controllers] var rightVBox: VBox = null
  @FXML private[Controllers] var trackButton: Button = null
  @FXML private[Controllers] var console: TextArea = null
  private var imageStack : Array[Array[Short]] = null
  private var foundSpots : Group = new Group
  private var foundTracks : Group = new Group
  private var tracks : Array[Array[Spot]] = null
  private var imageWidth : Int = 0
  private var imageHeight : Int = 0
  private val patchField : BigDecimalField = new BigDecimalField(BigDecimal.valueOf(1), new BigDecimal("1"), NumberFormat.getIntegerInstance())
  private val intensityThresholdField : BigDecimalField = new BigDecimalField(BigDecimal.valueOf(0.95), new BigDecimal("0.01"), new DecimalFormat("#,##0.00"))
  private val maxDistanceField : BigDecimalField = new BigDecimalField(BigDecimal.valueOf(10), new BigDecimal("1"), NumberFormat.getIntegerInstance())
  private val minFramesField : BigDecimalField = new BigDecimalField(BigDecimal.valueOf(1), new BigDecimal("1"), NumberFormat.getIntegerInstance())
  private val guessToggle : ToggleButton = {
    val tog = new ToggleButton("Manual Threshold")
    tog.setSelected(false)
    tog.setStyle("-fx-font-size: 10pt;");
    tog
  }

  implicit def bigDecToInt(in:BigDecimal) : Int = in.intValue()
  implicit def bigDecToDouble(in:BigDecimal) : Double = in.doubleValue()

  def initialize(url: URL, resourceBundle: ResourceBundle) = {
    println(this.getClass.getSimpleName + " initialized")
    anchorPane.getChildren.add(foundSpots)
    anchorPane.getChildren.add(foundTracks)
    frameSlider.valueProperty.addListener(new ChangeListener[Number] {
      def changed(arg0: ObservableValue[_ <: Number], arg1: Number, arg2: Number) {
        updateGraph(arg2.intValue)
      }
    })
    imageView.fitWidthProperty().bind(leftVBox.widthProperty())
    imageView.fitHeightProperty().bind(leftVBox.heightProperty())
    imageView.fitWidthProperty().addListener(new ChangeListener[Number] {
      def changed(p1 : ObservableValue[_ <: Number], p2 : Number, p3 : Number) {
        if(foundSpots.getChildren.isEmpty != true) updateSpots(frameSlider.getValue.toInt)
        if(tracks != null) redrawTracks
      }
    })
    imageView.fitHeightProperty().addListener(new ChangeListener[Number] {
      def changed(p1 : ObservableValue[_ <: Number], p2 : Number, p3 : Number) {
        if(foundSpots.getChildren.isEmpty != true) updateSpots(frameSlider.getValue.toInt)
        if(tracks != null) redrawTracks
      }
    })
    guessToggle.selectedProperty().addListener(new ChangeListener[Boolean] {
      def changed(p1 : ObservableValue[_ <: Boolean], p2 : Boolean, p3 : Boolean) {
        intensityThresholdField.setVisible(p3)
      }
    })
    patchField.numberProperty().addListener(new ChangeListener[BigDecimal] {
      def changed(p1 : ObservableValue[_ <:BigDecimal], p2 : BigDecimal, p3 : BigDecimal) {
        if(p3.intValue() < 1) patchField.setNumber(BigDecimal.valueOf(1))
        if(imageStack != null) updateSpots(frameSlider.getValue.toInt)
      }
    })
    rightVBox.setVisible(false)
    imageView.setSmooth(false)
    patchField.getStyleClass.add("bigDecimalField")
    intensityThresholdField.getStyleClass.add("bigDecimalField")
    maxDistanceField.getStyleClass.add("bigDecimalField")
    minFramesField.getStyleClass.add("bigDecimalField")
    val patchLabel = new Label("Patch Radius (px)")
    patchLabel.setStyle("-fx-font-size: 10pt;");
    rightVBox.getChildren.add(patchLabel)
    rightVBox.getChildren.add(patchField)
    val intensityLabel = new Label("Intensity Threshold (0-1)")
    intensityLabel.setStyle("-fx-font-size: 10pt;");
    intensityThresholdField.setVisible(false)
    rightVBox.getChildren.add(intensityLabel)
    rightVBox.getChildren.add(guessToggle)
    rightVBox.getChildren.add(intensityThresholdField)
    val diffLabel = new Label("Max. Diffusion Distance (px)")
    diffLabel.setStyle("-fx-font-size: 10pt;");
    rightVBox.getChildren.add(diffLabel)
    rightVBox.getChildren.add(maxDistanceField)
    val framesLabel = new Label("Min. Frames Track")
    framesLabel.setStyle("-fx-font-size: 10pt;");
    rightVBox.getChildren.add(framesLabel)
    rightVBox.getChildren.add(minFramesField)
    console.setVisible(false)
  }


  def exitApp(event:ActionEvent) : Unit = System.exit(0)

  private def outputConsole(s:String) : Unit = {
    if(console.getText == "") console.setText(s) else {
      val newString = console.getText + "\n" + s
      console.setText(newString)
    }
  }

  private def updateGraph(i: Int) = {
    imageStack == null match {
      case false => {
        val util: ImageUtils = new ImageUtils
        val img: Image = util.getJavaFXImage(imageStack(i), imageWidth, imageHeight)
        imageView.setImage(img)
        updateSpots(i)
      }
      case true => ()
    }
  }

  def loadImage(event: ActionEvent) = {
    val fc = new FileChooser
    val extension = new FileChooser.ExtensionFilter("TIFF files (*.tif)", "*.tif")
    fc.getExtensionFilters.add(extension)
    fc.setInitialDirectory(new File("/Users/James/Desktop/"))
    val file = fc.showOpenDialog(anchorPane.sceneProperty.get.getWindow)
    if (file != null) {
      val loaded: (Array[Array[Short]], Int, Int) = ImageLoad.load(file).get
      imageStack = loaded._1
      imageWidth = loaded._2
      imageHeight = loaded._3
      outputConsole(imageWidth.toString + " px wide, by " + imageHeight.toString + " px high")
      outputConsole(imageStack.length.toString + " frames loaded")
      frameSlider.setMax(imageStack.length - 1)
      val util: ImageUtils = new ImageUtils
      val img: Image = util.getJavaFXImage(imageStack(0), imageWidth, imageHeight)
      imageView.setImage(img)
    }
    else {
      outputConsole("Something wrong with that file... please try again.")
      ()
    }
    frameSlider.setVisible(true)
    trackButton.setVisible(true)
    rightVBox.setVisible(true)
    console.setVisible(true)
  }

  def redrawTracks = {
    foundTracks.getChildren.removeAll(foundTracks.getChildren)
    val xscale = imageView.getFitWidth / imageWidth
    val yscale = imageView.getFitHeight / imageHeight
    val paths = for(track <- tracks) yield {
      val path = new Path
      path.setStroke(Color.color(math.random,math.random,math.random))
      path.setStrokeWidth(1.0)
      path.setOpacity(0.5)
      path.getElements().add(new MoveTo(leftVBox.getLayoutX + (track(0).getX + 0.5) * xscale, leftVBox.getLayoutY + (track(0).getY + 0.5) * yscale))
      track.filter(track.indexOf(_) != 0).map(p => path.getElements().add(new LineTo(leftVBox.getLayoutX + (p.getX + 0.5) * xscale, leftVBox.getLayoutY + (p.getY + 0.5) * yscale)))
      path
    }
    paths.map(foundTracks.getChildren.add(_))
  }

  def track(event : ActionEvent) = {
    val tracker : Service[IndexedSeq[Path]] = new Service[IndexedSeq[Path]] {
      protected def createTask : Task[IndexedSeq[Path]] = {
        return new Task[IndexedSeq[Path]] {
          protected def call = {
            val thresh : Double = if(guessToggle.isSelected) {
              intensityThresholdField.getNumber
            } else 2.0
            tracks = Cleaner.cleanAllFrames(imageStack, imageWidth, imageHeight, patchField.getNumber, minFramesField.getNumber, maxDistanceField.getNumber, thresh)
            val xscale = imageView.getFitWidth / imageWidth
            val yscale = imageView.getFitHeight / imageHeight
            val paths = for(track <- tracks) yield {
              val path = new Path
              path.setStroke(Color.color(math.random,math.random,math.random))
              path.setStrokeWidth(1.0)
              path.setOpacity(0.5)
              path.getElements().add(new MoveTo(leftVBox.getLayoutX + (track(0).getX + 0.5) * xscale, leftVBox.getLayoutY + (track(0).getY + 0.5) * yscale))
              track.filter(track.indexOf(_) != 0).map(p => path.getElements().add(new LineTo(leftVBox.getLayoutX + (p.getX + 0.5) * xscale, leftVBox.getLayoutY + (p.getY + 0.5) * yscale)))
              updateProgress(tracks.indexOf(track).toLong, (tracks.length - 1).toLong)
              path
            }
            paths
          }
        }
      }
    }
    tracker.stateProperty.addListener(new ChangeListener[Worker.State] {
      def changed(observableValue : ObservableValue[_ <: Worker.State], oldState : Worker.State, newState : Worker.State) = {
        newState match {
          case Worker.State.SCHEDULED => foundTracks.getChildren.removeAll(foundTracks.getChildren)
          case Worker.State.READY => ()
          case Worker.State.RUNNING => outputConsole("Tracking...")
          case Worker.State.SUCCEEDED => {
            val output : IndexedSeq[Path] = tracker.valueProperty.getValue
            output.map(foundTracks.getChildren.add(_))
            progressBar.setVisible(false)
            progressBar.progressProperty().unbind()
            progressBar.setProgress(0)
            outputConsole("Tracking complete")
          }
          case Worker.State.CANCELLED => outputConsole("Tracking cancelled!")
          case Worker.State.FAILED => outputConsole("Tracking failed!")
          case _ => ()
        }
      }
    })
    progressBar.setProgress(0);
    progressBar.progressProperty().bind(tracker.progressProperty());
    progressBar.setVisible(true);
    tracker.start
  }

  private def updateSpots(i: Int) = {
    val thresh : Double = if(guessToggle.isSelected) {
      intensityThresholdField.getNumber
    } else 2.0
      val out = Cleaner.cleanFrame(imageStack(i), imageWidth, imageHeight, patchField.getNumber, thresh)
      foundSpots.getChildren.removeAll(foundSpots.getChildren)
      val xscale = imageView.getFitWidth / imageWidth
      val yscale = imageView.getFitHeight / imageHeight
      for(o <- out) {
        val r: Circle = new Circle()
        r.setCenterX(leftVBox.getLayoutX + ((o._1 + 0.5) * xscale))
        r.setCenterY(leftVBox.getLayoutY + ((o._2 + 0.5) * yscale))
        r.setRadius(patchField.getNumber.doubleValue() * xscale)
        r.setStrokeWidth(1.5)
        r.setStroke(Color.RED)
        r.setFill(Color.color(0, 0, 0, 0))
        r.setOpacity(1.0)
        val ft = new FadeTransition(Duration.millis(500), r)
        ft.setFromValue(1.0)
        ft.setToValue(0.1)
        ft.setCycleCount(Integer.MAX_VALUE)
        ft.setAutoReverse(true)
        ft.play()
        foundSpots.getChildren.add(r)
      }
  }

  /******************************************************************************************
   *                                    Initializables                                      *
   ******************************************************************************************/
  private def loadFXMLClass(fxml:String, title:String) : Initializable = {
    val loader = new FXMLLoader
    val in = classOf[Launch].getResourceAsStream(fxml)
    loader.setBuilderFactory(new JavaFXBuilderFactory)
    loader.setLocation(classOf[Launch].getResource(fxml))
    var page: AnchorPane = null
    try {
      page = loader.load(in).asInstanceOf[AnchorPane]
    }
    finally {
      in.close
    }
    val scene: Scene = new Scene(page)
    val stage: Stage = new Stage
    stage.setScene(scene)
    stage.sizeToScene
    stage.setTitle(title)
    stage.show
    loader.getController.asInstanceOf[Initializable]
  }

}