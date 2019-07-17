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

  val AllSquares: Range = 0 to 63

  // Squares
  val A1: Square = 0
  val B1: Square = 1
  val C1: Square = 2
  val D1: Square = 3
  val E1: Square = 4
  val F1: Square = 5
  val G1: Square = 6
  val H1: Square = 7
  val A2: Square = 8
  val B2: Square = 9
  val C2: Square = 10
  val D2: Square = 11
  val E2: Square = 12
  val F2: Square = 13
  val G2: Square = 14
  val H2: Square = 15
  val A3: Square = 16
  val B3: Square = 17
  val C3: Square = 18
  val D3: Square = 19
  val E3: Square = 20
  val F3: Square = 21
  val G3: Square = 22
  val H3: Square = 23
  val A4: Square = 24
  val B4: Square = 25
  val C4: Square = 26
  val D4: Square = 27
  val E4: Square = 28
  val F4: Square = 29
  val G4: Square = 30
  val H4: Square = 31
  val A5: Square = 32
  val B5: Square = 33
  val C5: Square = 34
  val D5: Square = 35
  val E5: Square = 36
  val F5: Square = 37
  val G5: Square = 38
  val H5: Square = 39
  val A6: Square = 40
  val B6: Square = 41
  val C6: Square = 42
  val D6: Square = 43
  val E6: Square = 44
  val F6: Square = 45
  val G6: Square = 46
  val H6: Square = 47
  val A7: Square = 48
  val B7: Square = 49
  val C7: Square = 50
  val D7: Square = 51
  val E7: Square = 52
  val F7: Square = 53
  val G7: Square = 54
  val H7: Square = 55
  val A8: Square = 56
  val B8: Square = 57
  val C8: Square = 58
  val D8: Square = 59
  val E8: Square = 60
  val F8: Square = 61
  val G8: Square = 62
  val H8: Square = 63

  val InvalidSquare: Square = -1
}
