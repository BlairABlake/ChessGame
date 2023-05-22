package com.blairablake

import com.blairablake.gui.GUIApp

object Main extends App {
  val game = Game.standardGame
  println(game)
  val gui = new GUIApp(600, 600)
  gui.startup(Array())
}
