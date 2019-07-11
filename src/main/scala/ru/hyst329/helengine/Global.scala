package ru.hyst329.helengine

object Global {
  type Piece         = Byte
  type Square        = Byte
  type BitBoard      = Long
  type CastlingFlags = Byte

  private implicit def intToByte(i: Int) = i.toByte

  val Empty: Piece       = 0
  val WhitePawn: Piece   = 1
  val WhiteKnight: Piece = 2
  val WhiteBishop: Piece = 3
  val WhiteRook: Piece   = 4
  val WhiteQueen: Piece  = 5
  val WhiteKing: Piece   = 6
  val BlackPawn: Piece   = 7
  val BlackKnight: Piece = 8
  val BlackBishop: Piece = 9
  val BlackRook: Piece   = 10
  val BlackQueen: Piece  = 11
  val BlackKing: Piece   = 12

  // Total count of piece types, including empty
  val PieceCount: Piece = 13
}
