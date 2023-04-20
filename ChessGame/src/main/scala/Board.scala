type _Board = Vector[Vector[Square]]

class Board(board: _Board) {
  def get(position: Position): Square = board(getY(position))(getX(position))
  def set(position: Position, square: Square): Board =
    println(getY(position))
    new Board(
      board.updated(
        getY(position),
        board(getY(position)).updated(
          getX(position),
          square
        )
      )
    )

  def getBoard(): _Board = board

  def getY(position: Position): Int = 8 - (position.y - 1) - 1
  def getX(position: Position): Int = position.x - 1
}
