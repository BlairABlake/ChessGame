package com.blairablake

import com.blairablake.Board._
import com.blairablake.Piece._

case class Game(
          wp: BitBoard,
          wr: BitBoard,
          wn: BitBoard,
          wb: BitBoard,
          wq: BitBoard,
          wk: BitBoard,
          bp: BitBoard,
          br: BitBoard,
          bn: BitBoard,
          bb: BitBoard,
          bq: BitBoard,
          bk: BitBoard,
          ) {
  
  def getPieces: PieceBoard =
    wp.toPieceBoard(Pawn(Color.White))
      .add(wr.toPieceBoard(Rook(Color.White)))
      .add(wn.toPieceBoard(Knight(Color.White)))
      .add(wb.toPieceBoard(Bishop(Color.White)))
      .add(wq.toPieceBoard(Queen(Color.White)))
      .add(wk.toPieceBoard(Knight(Color.White)))
      .add(bp.toPieceBoard(Pawn(Color.Black)))
      .add(br.toPieceBoard(Rook(Color.Black)))
      .add(bn.toPieceBoard(Knight(Color.Black)))
      .add(bb.toPieceBoard(Bishop(Color.Black)))
      .add(bq.toPieceBoard(Queen(Color.Black)))
      .add(bk.toPieceBoard(King(Color.Black)))
    
  def toStringList: List[String] =
    wp.toStringBoard("♙")
      .add(wr.toStringBoard("♖"))
      .add(wn.toStringBoard("♘"))
      .add(wb.toStringBoard("♗"))
      .add(wq.toStringBoard("♕"))
      .add(wk.toStringBoard("♔"))
      .add(bp.toStringBoard("♟︎"))
      .add(br.toStringBoard("♜"))
      .add(bn.toStringBoard("♞"))
      .add(bb.toStringBoard("♝"))
      .add(bq.toStringBoard("♛"))
      .add(bk.toStringBoard("♚"))
      .getBoard
  
  override def toString: String =
    toStringList
      .map(e =>
        if (e == "") "."
        else e
      )
      .reverse
      .zipWithIndex
      .foldLeft("")((s, li) => li match {
        case (e, i) =>
          if (i % 8 == 7) s+e+"\n"
          else s+e
      })

}

object Game {
  val standardGame = new Game(
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i >= 8 & i < 16) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 0 | i == 7) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 1 | i == 6) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 2 | i == 5) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 3) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 4) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i >= 48  & i < 56) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 56 | i == 63) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 57 | i == 62) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 58 | i == 61) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 59) 1 else e)),
    BitBoard(List.fill(64)(0).zipWithIndex.map((e, i) => if (i == 60) 1 else e)),
  )

  def move(game: Game, prevPos: Int, nextPos: Int): Game =
    game.getPieces.getBoard(prevPos) match {
      case Pawn(Color.White) =>
        new Game(
          wp = game.wp.updated(prevPos, nextPos),
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
      case Rook(Color.White) =>
        new Game(
          wp = game.wp,
          wr = game.wr.updated(prevPos, nextPos),
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
      case Knight(Color.White) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn.updated(prevPos, nextPos),
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
      case Bishop(Color.White) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb.updated(prevPos, nextPos),
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
      case Queen(Color.White) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq.updated(prevPos, nextPos),
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
      case King(Color.White) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk.updated(prevPos, nextPos),
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
      case Pawn(Color.Black) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp.updated(prevPos, nextPos),
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
      case Rook(Color.Black) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br.updated(prevPos, nextPos),
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
      case Knight(Color.Black) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn.updated(prevPos, nextPos),
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
      case Bishop(Color.Black) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb.updated(prevPos, nextPos),
          bq = game.bq,
          bk = game.bk
        )
      case Queen(Color.Black) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq.updated(prevPos, nextPos),
          bk = game.bk
        )
      case Knight(Color.Black) =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk.updated(prevPos, nextPos)
        )
      case EmptyPiece =>
        new Game(
          wp = game.wp,
          wr = game.wr,
          wb = game.wb,
          wn = game.wn,
          wq = game.wq,
          wk = game.wk,
          bp = game.bp,
          br = game.br,
          bn = game.bn,
          bb = game.bb,
          bq = game.bq,
          bk = game.bk
        )
    }
}
