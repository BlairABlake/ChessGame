package com.blairablake.gui

import com.blairablake.Game
import com.blairablake.Piece.*

import java.awt.geom.GeneralPath
import java.awt.Color
import scala.swing.Swing.*
import scala.swing.event.*
import scala.swing.*
import scala.collection.mutable
import scala.language.postfixOps

class GUIApp(width: Int, height: Int) extends SimpleSwingApplication {
  var game: Game = Game.standardGame
  var colors: mutable.Seq[Color] = mutable.Seq.fill(64)(Color(171, 105, 43)).zipWithIndex.map((c, i) =>
    if (i/8%2 == 0 & i %2 == 0) Color(217, 152, 91)
    else if (i/8%2 == 1 & i %2 == 1)Color(217, 152, 91)
    else c
  )

  def top: Frame = new MainFrame {
    title = "Chess"
    preferredSize = (width, height)

    val components: Seq[Label] = game.toStringList.reverse.zipWithIndex.map(s =>
      new Label {
        val index = s(1)
        background = colors(index)
        text = s(0)
        font = new Font("Robot", 0, 64)
        opaque = true
        listenTo(mouse.clicks)
        listenTo(mouse.moves)


        reactions += {
          case e: MouseClicked =>
          case e: MouseEntered => updateColor(index, Color(179, 125, 75))
          case e: MouseExited => updateColor(index, colors(index))
        }
      })

    def updateGame(game: Game): Unit =
      this.contents = new GridPanel(8, 8) {
        components.foreach(l =>
          this.contents += l
        )
      }

    def updateColor(i: Int, color: Color): Unit =
      components(i).background = color

    updateGame(game)
  }
}
