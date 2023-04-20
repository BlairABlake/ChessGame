type _Board = Vector[Vector[Square]]

class Board(board: _Board) {
  def get(position: Position): Square = board(position.Y())(position.X())
  def set(position: Position, square: Square): Board =
    new Board(
      board.updated(
        position.Y(),
        board(position.Y()).updated(
          position.X(),
          square
        )
      )
    )

  def getBoard(): _Board = board
}
