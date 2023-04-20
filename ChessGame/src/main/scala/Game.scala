import Color.{Gray, White}

class Game(players: List[Player], board: Board, state: State) {
  val winner: Option[Player] = None

  def move(movement: Move, player: Player): Game = {
    if (!validateMove(movement, player)) new Game(players, board, state)
    else new Game(
      players,
      board
        .set(movement.nextPos, board.get(movement.prevPos))
        .set(movement.prevPos, new Square(Piece.Empty, Gray)),
      changeState(state)
    )
  }

  def validateMove(movement: Move, player: Player): Boolean = {
    if (!players.contains(player)) false
    else if (player.color != board.get(movement.prevPos).ownedBy) false
    else true
  }

  def changeState(state: State): State = state match {
    case State.White => State.Black
    case State.Black => State.White
  }

  override def toString: String = {
    board.getBoard().foldLeft("")(
      (s: String, row: Vector[Square]) => s + row.foldLeft("")((s: String, square: Square) => s+square.toString) + "\n"
    )
  }
}
