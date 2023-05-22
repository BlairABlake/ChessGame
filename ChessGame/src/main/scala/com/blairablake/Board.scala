package com.blairablake



object Board {
  import com.blairablake.Piece._

  trait Board {
  }

  class BitBoard(board: List[Int]) extends Board {
    def add(other: BitBoard): BitBoard =
      new BitBoard(board.zip(other.getBoard).map((e1, e2) => if (e1 == 1 | e2 == 1) 1 else 0))
      
    def updated(prevPos: Int, nextPos: Int) = new BitBoard(board.updated(prevPos, 0).updated(nextPos, 1))

    def getBoard: List[Int] = board

    def toStringBoard(s: String): StringBoard = new StringBoard(board.map(e => if(e == 1) s else ""))
    def toPieceBoard(p: Piece): PieceBoard = new PieceBoard(board.map(e => if(e == 1) p else EmptyPiece))
  }

  class StringBoard(board: List[String]) extends Board {
    def add(other: StringBoard): StringBoard =
      new StringBoard(board.zip(other.getBoard).map((e1, e2) =>
        if (e1 != "") e1
        else if (e2 != "") e2
        else ""
      ))

    def getBoard: List[String] = board
  }

  class PieceBoard(board: List[Piece]) extends Board {
    def add(other: PieceBoard): PieceBoard =
      new PieceBoard(board.zip(other.getBoard).map((e1, e2) =>
        if(e1 != EmptyPiece) e1
        else if (e2 != EmptyPiece) e2
        else EmptyPiece
      ))
      
    def updated(prevPos: Int, nextPos: Int) = toBitBoard.updated(prevPos, nextPos).toPieceBoard(getPiece)
      
    def toBitBoard: BitBoard = new BitBoard(board.map(p => if(p == EmptyPiece) 0 else 1))

    def getBoard: List[Piece] = board
    
    def getPiece: Piece = 
      board.filter(p => p != EmptyPiece) match {
        case x :: xs => x
        case _ => EmptyPiece
      }
  }
}