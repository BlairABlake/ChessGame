@main def run(): Unit = {
  val game = new Game(
    List(new Player(Color.White), new Player(Color.Black)),
    GameFactory.makeBasicBoard(),
    State.White
  )
  println(game.move(Move(Position.fromAlg("a2"), Position.fromAlg("a3")), new Player(Color.White)))
}