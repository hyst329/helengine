package ru.hyst329.helengine

import ru.hyst329.helengine.Global._

class Board {
  var bitBoards: List[BitBoard] = List.fill(PieceCount)(0x0l)
  val enPassantSquare: Square = 0.toByte
}
