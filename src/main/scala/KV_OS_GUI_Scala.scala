import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text

class Tlacitko(X: Int, Y: Int) extends Rectangle:
    width = 100
    height = 100
    x = X
    y = Y
    fill = Color.rgb(0, 255, 0)

class ShutdownButton(X: Int, Y: Int) extends Rectangle:
    width = 80
    height = 40
    x = X / 2 - width.toInt * 1.3
    y = Y / 2 - height.toInt / 2

class ImportantText(txt: String, X: Int) extends Text(txt):
    style = "-fx-font: bold 20pt monospace"
    fill = Color.Green
    layoutX = X
    layoutY = 180

class SmallText(txt: String, shutdownTextY: Double) extends Text(txt):
    fill = Color.White
    style = "-fx-font: 10pt calibri"
    y = shutdownTextY * 1.7

trait konstrukceKarty(kolikKaret: Option[Double]) extends Rectangle:
    fill = Color.rgb(0, 100, 0)
    width = 100
    height = if kolikKaret.isDefined then 20 else 30
    x = kolikKaret.getOrElse(0.37) * width.toInt
    y = if kolikKaret.isDefined then 550 else 235

class GamesButton extends konstrukceKarty(None)

abstract class Karta(kolikKaret: Option[Double]) extends konstrukceKarty(kolikKaret):
    def text: String
    override def toString = text
    def highlight(): Unit =
        fill = Color.rgb(0, 255, 0)
    def dehighlight(): Unit =
        fill = Color.rgb(0, 100, 0)

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
            val txt = HorniText("KV OS!", X)

            var shutdownTlacitkoZapnute = false
            var gamesTlacitkoZapnute = false
            var optionsTlacitkoZapnute = false

            val tlacitko: Rectangle = new Tlacitko(40, 50):
                onMouseClicked = () => shutdownClick()

            val shutdownTextY: Double = 80 + tlacitko.getHeight
            val shutdownText: Text = new ImportantText("SHUTDOWN", 27)

            val ShutdownButtonAno: ShutdownButton = new ShutdownButton(X, Y):
                onMouseClicked = () => System.exit(0)
            val ShutdownButtonAnoText: Text = new SmallText("Ano", shutdownTextY):
                onMouseClicked = () => System.exit(0)
            ShutdownButtonAnoText.x = centerX(X, ShutdownButtonAnoText, 3)

            val ShutdownButtonNe: Rectangle = new ShutdownButton(X, Y):
                x = x.toInt + width.toInt * 1.5
                onMouseClicked = () => shutdownClick()
            val ShutdownButtonNeText: Text = new SmallText("Ne", shutdownTextY):
                onMouseClicked = () => shutdownClick()
            ShutdownButtonNeText.x = centerX(X, ShutdownButtonNeText, -2.7)

            val shutdownClick = () =>
                if !optionsTlacitkoZapnute then
                    tlacitko.fill = if shutdownTlacitkoZapnute then Color.rgb(0, 255, 0) else Color.rgb(255, 255, 255)
                    content = if !shutdownTlacitkoZapnute then shutdownDesktop else defaultDesktop
                    shutdownTlacitkoZapnute = !shutdownTlacitkoZapnute

            val gamesButton: Rectangle = GamesButton()

            val games_txt: Text = HorniText("Games", X)
            val gamesText: Text = new Text("GAMES"):
                style = "-fx-font: bold 20pt monospace"
                fill = Color.rgb(0, 170, 0)
                layoutX = 47
                layoutY = shutdownTextY + 80
                onMouseClicked = () => gamesClick()

            var pocetKaret = -1
            var gamesKarta = GamesKarta(Option(0))
            var gamesKartaText: Text = new Text()
            var gamesOpened = false
            var gamesHighlighted = false
            val gamesClick = () =>
                if shutdownTlacitkoZapnute then shutdownClick()
                if optionsTlacitkoZapnute then optionsClick()
                pocetKaret = if !gamesOpened then pocetKaret + 1 else pocetKaret
                gamesKarta = GamesKarta(Option(pocetKaret))

                gamesOpened = true
                gamesHighlighted = true

                val gamesExit = new Text("Exit"):
                    style = "-fx-font: 15pt sains-serif"
                    layoutX = 730
                    layoutY = 550
                    onMouseClicked = () =>
                        gamesHighlighted = false
                        gamesOpened = false
                        pocetKaret -= 1
                        defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, optionsTlacitko, optionsText)
                        content = defaultDesktop

                gamesKartaText = new Text(gamesKarta.toString):
                    style = "-fx-font: 10pt calibri"
                    layoutX = gamesKarta.x.toInt + gamesKarta.width.toInt - 75
                    layoutY = 565
                    onMouseClicked = () =>
                        gamesHighlighted = !gamesHighlighted
                        if gamesHighlighted then gamesKarta.highlight() else gamesKarta.dehighlight()
                        defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, gamesKarta, this, optionsTlacitko, optionsText)
                        content = if !gamesHighlighted then defaultDesktop else List(games_txt, gamesKarta, this, gamesExit)

                gamesKarta.onMouseClicked = () =>
                    gamesHighlighted = !gamesHighlighted
                    if gamesHighlighted then gamesKarta.highlight() else gamesKarta.dehighlight()
                    defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, gamesKarta, gamesKartaText, optionsTlacitko, optionsText)
                    content = if !gamesHighlighted then defaultDesktop else List(games_txt, gamesKarta, gamesKartaText, gamesExit)
                gamesKarta.highlight()

                content = List(games_txt, gamesKarta, gamesKartaText, gamesExit)

            val optionsTlacitko: Rectangle = new Tlacitko(200, 50):
                onMouseClicked = () => optionsClick()
            val optionsText: Text = new ImportantText("OPTIONS", 194)

            val optionsLanguage: Rectangle = new ShutdownButton(X, Y):
                onMouseClicked = () =>
                    println("WIP!")
            val optionsLanguageText: Text = new SmallText("Options", shutdownTextY):
                onMouseClicked = () =>
                    println("WIP!")
            optionsLanguageText.x = centerX(X, optionsLanguageText, 1.8)

            val optionsBack: Rectangle = new ShutdownButton(X, Y):
                x = x.toInt + width.toInt * 1.5
                onMouseClicked = () => optionsClick()
            val optionsBackText: Text = new SmallText("Back", shutdownTextY):
                onMouseClicked = () => optionsClick()
            optionsBackText.x = centerX(X, optionsBackText, -1.3)

            val optionsClick = () =>
                if !shutdownTlacitkoZapnute then
                    optionsTlacitko.fill = if optionsTlacitkoZapnute then Color.rgb(0, 255, 0) else Color.rgb(255, 255, 255)
                    content = if !optionsTlacitkoZapnute then optionsDesktop else defaultDesktop
                    optionsTlacitkoZapnute = !optionsTlacitkoZapnute

            val shutdownDesktop = List(txt, tlacitko, shutdownText, ShutdownButtonAno, ShutdownButtonNe, ShutdownButtonAnoText, ShutdownButtonNeText, gamesButton, gamesText, optionsTlacitko, optionsText)
            val optionsDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, optionsTlacitko, optionsText, optionsLanguage, optionsBack, optionsLanguageText, optionsBackText)
            var defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, optionsTlacitko, optionsText)
            content = defaultDesktop