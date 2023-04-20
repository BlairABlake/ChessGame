enum Piece(val text: String):
  case Empty extends Piece(" ")
  case Pawn extends Piece("♙")
  case Bishop extends Piece("♗")
  case Knight extends Piece("♘")
  case Rook extends Piece("♖")
  case Queen extends Piece("♕")
  case King extends Piece("♔")

  override def toString: String = text
