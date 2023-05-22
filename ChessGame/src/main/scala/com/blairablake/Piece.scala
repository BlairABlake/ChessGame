package com.blairablake

object Piece {
  import com.blairablake.Color._
  
  sealed trait Piece
  case object EmptyPiece extends Piece
  case class Pawn(color: Color) extends Piece
  case class Rook(color: Color) extends Piece
  case class Knight(color: Color) extends Piece
  case class Bishop(color: Color) extends Piece
  case class Queen(color: Color) extends Piece
  case class King(color: Color) extends Piece
}