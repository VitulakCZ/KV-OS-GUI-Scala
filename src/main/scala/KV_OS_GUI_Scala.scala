import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text

class ShutdownButton(X: Int, Y: Int) extends Rectangle:
    width = 80
    height = 40
    x = X / 2 - width.toInt * 1.3
    y = Y / 2 - height.toInt / 2

trait konstrukceKarty(kolikKaret: Option[Double]) extends Rectangle:
    width = 100
    height = if kolikKaret.isDefined then 20 else 30
    fill = if kolikKaret.isDefined then Color.Red else Color.rgb(0, 100, 0)
    x = kolikKaret.getOrElse(0.37) * width.toInt
    y = if kolikKaret.isDefined then 550 else 235

class GamesButton extends konstrukceKarty(None)

abstract class Karta(kolikKaret: Option[Double]) extends konstrukceKarty(kolikKaret):
    def text: String
    override def toString = text
    def click(): Unit = println("Karta click")

class GamesKarta(kolikKaret: Option[Double]) extends Karta(kolikKaret):
    val text = "Games"

val centerX = (X: Int, txt: Text, multiplier: Number) => X / 2 - txt.getLayoutBounds.getWidth * multiplier.floatValue()

class HorniText(txt: String, X: Int) extends Text(txt):
    style = "-fx-font: bold 30pt monospace"
    x = centerX(X, this, 1.5)
    layoutY = 40

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
            val txt = new HorniText("KV OS!", X)

            var shutdownTlacitkoZapnute = false
            var gamesTlacitkoZapnute = false

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

            val ShutdownButtonAno: ShutdownButton = new ShutdownButton(X, Y):
                onMouseClicked = () => System.exit(0)
            val ShutdownButtonAnoText: Text = new Text("Ano"):
                fill = Color.White
                style = "-fx-font: 10pt calibri"
                y = shutdownTextY * 1.7
                onMouseClicked = () => System.exit(0)
            ShutdownButtonAnoText.x = centerX(X, ShutdownButtonAnoText, 3)

            val ShutdownButtonNe: Rectangle = new ShutdownButton(X, Y):
                x = x.toInt + width.toInt * 1.5
                onMouseClicked = () => shutdownClick()
            val ShutdownButtonNeText: Text = new Text("Ne"):
                fill = Color.White
                style = "-fx-font: 10pt calibri"
                y = shutdownTextY * 1.7
                onMouseClicked = () => shutdownClick()
            ShutdownButtonNeText.x = centerX(X, ShutdownButtonNeText, -2.7)

            val shutdownClick = () =>
                tlacitko.fill = if shutdownTlacitkoZapnute then Color.rgb(0, 255, 0) else Color.rgb(255, 255, 255)
                content = if !shutdownTlacitkoZapnute then shutdownDesktop else defaultDesktop
                shutdownTlacitkoZapnute = !shutdownTlacitkoZapnute

            val gamesButton: Rectangle = new GamesButton()
            
            val games_txt: Text = new HorniText("Games", X)
            val gamesText: Text = new Text("GAMES"):
                style = "-fx-font: bold 20pt monospace"
                fill = Color.rgb(0, 170, 0)
                layoutX = 48
                layoutY = shutdownTextY + 80
                onMouseClicked = () => gamesClick()

            var gamesKarta = GamesKarta(Option(0))
            val gamesClick = () =>
                if shutdownTlacitkoZapnute then shutdownClick()
                gamesKarta = GamesKarta(Option(0))
                val gamesKartaText = new Text(gamesKarta.toString):
                    style = "-fx-font: 10pt calibri"
                    println(gamesKarta.x.toInt + gamesKarta.width.toInt - 20)
                    layoutX = gamesKarta.x.toInt + gamesKarta.width.toInt - 75
                    layoutY = 565
                content = List(games_txt, gamesKarta, gamesKartaText)

            val shutdownDesktop = List(txt, tlacitko, shutdownText, ShutdownButtonAno, ShutdownButtonNe, ShutdownButtonAnoText, ShutdownButtonNeText, gamesButton, gamesText)
            val defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText)
            content = defaultDesktop