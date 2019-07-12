package ru.hyst329.helengine

import ru.hyst329.helengine.Global._

case class Board(var bitBoards: Array[BitBoard],
                 var enPassantSquare: Square,
                 var castlingFlags: CastlingFlags,
                 var moveNumber: Int,
                 var halfmoveCounter: Int,
                 var whiteToMove: Boolean) {

  // Occupied squares bit boards
  def occupationWhite: BitBoard =
    bitBoards(WhitePawn) | bitBoards(WhiteKnight) | bitBoards(WhiteBishop) |
      bitBoards(WhiteRook) | bitBoards(WhiteQueen) | bitBoards(WhiteKing)

  def occupationBlack: BitBoard =
    bitBoards(BlackPawn) | bitBoards(BlackKnight) | bitBoards(BlackBishop) |
      bitBoards(BlackRook) | bitBoards(BlackQueen) | bitBoards(BlackKing)

  def occupationAll: BitBoard = occupationWhite | occupationBlack
}


object Board {
  def fromFEN(fen: String): Option[Board] = {
    new FENParser(fen).position.run().toOption
  }
}