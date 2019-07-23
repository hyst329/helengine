package ru.hyst329.helengine

import ru.hyst329.helengine.Global._

case class Move(
    from: Square,
    to: Square,
    captures: Piece,
    oldEnPassant: Square,
    oldCastling: CastlingFlags,
    oldHalfmoveCounter: Int,
    promotesTo: Piece = Empty
) {
  def castlingToPlain: Option[Move] = this match {
    case Move(_from, _to, Empty, _, _oldCastling, _, Empty)
        if _from == E1 && _to == G1 && (_oldCastling & WhiteKingSide) != 0 =>
      Some(this.copy(to = F1))
    case Move(_from, _to, Empty, _, _oldCastling, _, Empty)
        if _from == E1 && _to == C1 && (_oldCastling & WhiteQueenSide) != 0 =>
      Some(this.copy(to = D1))
    case Move(_from, _to, Empty, _, _oldCastling, _, Empty)
        if _from == E8 && _to == G8 && (_oldCastling & BlackKingSide) != 0 =>
      Some(this.copy(to = F8))
    case Move(_from, _to, Empty, _, _oldCastling, _, Empty)
        if _from == E8 && _to == C8 && (_oldCastling & BlackQueenSide) != 0 =>
      Some(this.copy(to = D8))
    case _ => None
  }
}

object Move {
  val PieceAbbreviations: Map[Piece, String] = Map(
    Empty       -> "",
    WhitePawn   -> "",
    WhiteKnight -> "N",
    WhiteBishop -> "B",
    WhiteRook   -> "R",
    WhiteQueen  -> "Q",
    WhiteKing   -> "K",
    BlackPawn   -> "",
    BlackKnight -> "N",
    BlackBishop -> "B",
    BlackRook   -> "R",
    BlackQueen  -> "Q",
    BlackKing   -> "K"
  )

  def squareToAlgebraic(square: Square) = s"${('a' + square % 8).toChar}${square / 8 + 1}"

  def fromBoardContext(board: Board, from: Square, to: Square, promotesTo: Piece = Empty) = Move(
    from,
    to,
    board.getPiece(to),
    board.enPassantSquare,
    board.castlingFlags,
    board.halfmoveCounter,
    promotesTo
  )

  def toNotation(move: Move, board: Board): String = {
    val fromPiece = board.getPiece(move.from)
    val toPiece   = board.getPiece(move.to)
    toPiece match {
      case Empty =>
        s"${PieceAbbreviations(fromPiece)}${squareToAlgebraic(move.from)}-${squareToAlgebraic(move.to)}"
      case _ =>
        s"${PieceAbbreviations(fromPiece)}${squareToAlgebraic(move.from)}x${squareToAlgebraic(move.to)}"
    }
  }
}
