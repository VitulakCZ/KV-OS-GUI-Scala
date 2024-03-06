import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text

class shutdownButton(X: Int, Y: Int) extends Rectangle:
    width = 80
    height = 40
    x = X / 2 - width.toInt * 1.3
    y = Y / 2 - height.toInt / 2

val centerX = (X: Int, txt: Text, multiplier: Number) => X / 2 - txt.getLayoutBounds.getWidth * multiplier.floatValue()

object KV_OS_GUI_Scala extends JFXApp3:
    def start(): Unit =
        val X = 800
        val Y = 600
        stage = new JFXApp3.PrimaryStage():
            title = "KV OS GUI"
            width = X
            height = Y
        stage.setResizable(false)

        stage.scene = new Scene:
            fill = Color.rgb(0, 0, 255)
            val txt = new Text("KV OS!")
            txt.style = "-fx-font: bold 30pt monospace"
            txt.x = centerX(X, txt, 1.5)
            txt.layoutY = 40

            var tlacitkoZapnute = false
            val tlacitko: Rectangle = new Rectangle:
                width = 100
                height = 100
                x = 40
                y = 50
                fill = Color.rgb(0, 255, 0)
                onMouseClicked = () => shutdownClick()
            val shutdownTextY: Double = 80 + tlacitko.getHeight

            val shutdownText: Text = new Text("SHUTDOWN"):
                style = "-fx-font: bold 20pt monospace"
                fill = Color.Green
                layoutX = 25
                layoutY = shutdownTextY

            val shutdownButtonAno: shutdownButton = new shutdownButton(X, Y):
                onMouseClicked = () => System.exit(0)
            val shutdownButtonAnoText: Text = new Text("Ano"):
                fill = Color.White
                style = "-fx-font: 10pt calibri"
                y = shutdownTextY * 1.7
                onMouseClicked = () => System.exit(0)
            shutdownButtonAnoText.x = centerX(X, shutdownButtonAnoText, 3)

            val shutdownButtonNe: Rectangle = new shutdownButton(X, Y):
                x = x.toInt + width.toInt * 1.5
                onMouseClicked = () => shutdownClick()
            val shutdownButtonNeText: Text = new Text("Ne"):
                fill = Color.White
                style = "-fx-font: 10pt calibri"
                y = shutdownTextY * 1.7
                onMouseClicked = () => shutdownClick()
            shutdownButtonNeText.x = centerX(X, shutdownButtonNeText, -2.7)

            val shutdownClick = () =>
                tlacitko.fill = if tlacitkoZapnute then Color.rgb(0, 255, 0) else Color.rgb(255, 255, 255)
                content = if !tlacitkoZapnute then shutdownDesktop else defaultDesktop
                tlacitkoZapnute = !tlacitkoZapnute

            val shutdownDesktop = List(txt, tlacitko, shutdownText, shutdownButtonAno, shutdownButtonNe, shutdownButtonAnoText, shutdownButtonNeText)
            val defaultDesktop = List(txt, tlacitko, shutdownText)
            content = defaultDesktop