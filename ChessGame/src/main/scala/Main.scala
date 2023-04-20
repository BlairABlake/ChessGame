@main def run(): Unit = {
  val game = new Game(
    List(new Player(Color.White), new Player(Color.Black)),
    GameFactory.makeBasicBoard(),
    State.White
  )

  println(game)
}