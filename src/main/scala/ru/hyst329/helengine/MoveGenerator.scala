package ru.hyst329.helengine

import ru.hyst329.helengine.Global._

object MoveGenerator {
  def generatePseudoLegal(board: Board): List[Move] = {
    val result: List[Move] = List()
    val (pawn, knight, rook, bishop, queen, king) = if (board.whiteToMove) {
      (WhitePawn, WhiteKnight, WhiteRook, WhiteBishop, WhiteQueen, WhiteKing)
    } else {
      (BlackPawn, BlackKnight, BlackRook, BlackBishop, BlackQueen, BlackKing)
    }
    val occupationCurrent  = board.occupationCurrentSide
    val occupationOpposite = board.occupationOppositeSide
    val occupationAll      = board.occupationAll
    (0 to 63).foreach { square =>
      board.getPiece(square.toByte) match {
        case pawn =>
        // Generate pawn moves
        case knight =>
        // Generate knight moves
        case bishop =>
        // Generate bishop moves
        case rook =>
        // Generate rook moves
        case queen =>
        // Generate queen moves
        case king =>
        // Generate king moves
        case _ =>
        // Do nothing, either an opponent's piece or empty square
      }
    }
    result
  }
}
