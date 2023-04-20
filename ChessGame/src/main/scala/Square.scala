case class Square(piece: Piece, ownedBy: Color) {
  override def toString: String = piece.toString
}
