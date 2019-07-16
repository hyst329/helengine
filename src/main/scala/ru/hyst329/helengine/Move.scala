package ru.hyst329.helengine

import ru.hyst329.helengine.Global._

case class Move(
    from: Square,
    to: Square,
    captures: Piece,
    oldEnPassant: Square,
    oldCastling: CastlingFlags,
    promotesTo: Piece = Empty
)

object Move {
  def fromBoardContext(board: Board, from: Square, to: Square, promotesTo: Piece = Empty) = Move(
    from, to, board.getPiece(to), board.enPassantSquare, board.castlingFlags, promotesTo
  )
}