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

class LanguageKarta(kolikKaret: Option[Double]) extends Karta(kolikKaret):
    val text = "Language"

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
                        if !languageOpened then
                            defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, optionsTlacitko, optionsText)
                        else
                            defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, optionsTlacitko, optionsText, languageKarta, languageKartaText)
                        content = defaultDesktop

                gamesKartaText = new Text(gamesKarta.toString):
                    style = "-fx-font: 10pt calibri"
                    layoutX = gamesKarta.x.toInt + gamesKarta.width.toInt - 75
                    layoutY = 565
                    onMouseClicked = () =>
                        gamesHighlighted = !gamesHighlighted
                        languageHighlighted = false
                        languageKarta.dehighlight()
                        if gamesHighlighted then gamesKarta.highlight() else gamesKarta.dehighlight()
                        if !languageOpened then
                            defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, gamesKarta, this, optionsTlacitko, optionsText)
                        else
                            defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, gamesKarta, this, optionsTlacitko, optionsText, languageKarta, languageKartaText)
                        if !gamesHighlighted then
                            content = defaultDesktop
                        else if gamesHighlighted && !languageOpened then
                            content = List(games_txt, gamesKarta, gamesKartaText, gamesExit)
                        else if languageOpened then
                            content = List(games_txt, gamesKarta, gamesKartaText, gamesExit, languageKarta, languageKartaText)

                gamesKarta.onMouseClicked = () =>
                    gamesHighlighted = !gamesHighlighted
                    languageHighlighted = false
                    languageKarta.dehighlight()
                    if gamesHighlighted then gamesKarta.highlight() else gamesKarta.dehighlight()
                    if !languageOpened then
                        defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, gamesKarta, gamesKartaText, optionsTlacitko, optionsText)
                    else
                        defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, gamesKarta, gamesKartaText, optionsTlacitko, optionsText, languageKarta, languageKartaText)
                    if !gamesHighlighted then
                        content = defaultDesktop
                    else if gamesHighlighted && !languageOpened then
                            content = List(games_txt, gamesKarta, gamesKartaText, gamesExit)
                    else if languageOpened then
                        content = List(games_txt, gamesKarta, gamesKartaText, gamesExit, languageKarta, languageKartaText)
                gamesKarta.highlight()

                if !languageOpened then
                    content = List(games_txt, gamesKarta, gamesKartaText, gamesExit)
                else
                    content = List(games_txt, gamesKarta, gamesKartaText, gamesExit, languageKarta, languageKartaText)

            val optionsTlacitko: Rectangle = new Tlacitko(200, 50):
                onMouseClicked = () => optionsClick()

            val options_txt: Text = HorniText("Options", X)
            val optionsText: Text = new ImportantText("OPTIONS", 194)

            val optionsLanguage: Rectangle = new ShutdownButton(X, Y):
                onMouseClicked = () => languageClick()
            val optionsLanguageText: Text = new SmallText("Language", shutdownTextY):
                onMouseClicked = () => languageClick()
            optionsLanguageText.x = centerX(X, optionsLanguageText, 1.5)

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

            var languageKarta: LanguageKarta = LanguageKarta(Option(pocetKaret))
            var languageKartaText: Text = new Text()
            var languageOpened = false
            var languageHighlighted = false
            val languageClick = () =>
                if shutdownTlacitkoZapnute then shutdownClick()
                if optionsTlacitkoZapnute then optionsClick()
                pocetKaret = if !languageOpened then pocetKaret + 1 else pocetKaret
                languageKarta = LanguageKarta(Option(pocetKaret))

                languageOpened = true
                languageHighlighted = true

                val languageExit = new Text("Exit"):
                    style = "-fx-font: 15pt sains-serif"
                    layoutX = 730
                    layoutY = 550
                    onMouseClicked = () =>
                        languageHighlighted = false
                        languageOpened = false
                        pocetKaret -= 1
                        if !gamesOpened then
                            defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, optionsTlacitko, optionsText)
                        else
                            defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, optionsTlacitko, optionsText, gamesKarta, gamesKartaText)
                        content = defaultDesktop

                languageKartaText = new Text(languageKarta.toString):
                    style = "-fx-font: 10pt calibri"
                    layoutX = languageKarta.x.toInt + languageKarta.width.toInt - 75
                    layoutY = 565
                    onMouseClicked = () =>
                        languageHighlighted = !languageHighlighted
                        gamesKarta.dehighlight()
                        gamesHighlighted = false
                        if languageHighlighted then languageKarta.highlight() else languageKarta.dehighlight()
                        if !gamesOpened then
                            defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, languageKarta, this, optionsTlacitko, optionsText)
                        else
                            defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, languageKarta, this, optionsTlacitko, optionsText, gamesKarta, gamesKartaText)
                        if !languageHighlighted then
                            content = defaultDesktop
                        else if languageHighlighted && !gamesOpened then
                            content = List(options_txt, languageKarta, languageKartaText, languageExit)
                        else if gamesOpened then
                            content = List(options_txt, languageKarta, languageKartaText, languageExit, gamesKarta, gamesKartaText)
                languageKarta.onMouseClicked = () =>
                    languageHighlighted = !languageHighlighted
                    gamesKarta.dehighlight()
                    gamesHighlighted = false
                    if languageHighlighted then languageKarta.highlight() else languageKarta.dehighlight()
                    if !gamesOpened then
                        defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, languageKarta, languageKartaText, optionsTlacitko, optionsText)
                    else
                        defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, gamesKarta, gamesKartaText, optionsTlacitko, optionsText, languageKarta, languageKartaText)
                    if !languageHighlighted then
                        content = defaultDesktop
                    else if languageHighlighted && !gamesOpened then
                            content = List(options_txt, languageKarta, languageKartaText, languageExit)
                    else if gamesOpened then
                        content = List(options_txt, languageKarta, languageKartaText, languageExit, gamesKarta, gamesKartaText)

                languageKarta.highlight()
                if !gamesOpened then
                    content = List(options_txt, languageKarta, languageKartaText, languageExit)
                else
                    content = List(options_txt, languageKarta, languageKartaText, languageExit, gamesKarta, gamesKartaText)
            val shutdownDesktop = List(txt, tlacitko, shutdownText, ShutdownButtonAno, ShutdownButtonNe, ShutdownButtonAnoText, ShutdownButtonNeText, gamesButton, gamesText, optionsTlacitko, optionsText)
            val optionsDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, optionsTlacitko, optionsText, optionsLanguage, optionsBack, optionsLanguageText, optionsBackText)
            var defaultDesktop = List(txt, tlacitko, shutdownText, gamesButton, gamesText, optionsTlacitko, optionsText)
            content = defaultDesktop