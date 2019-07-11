package ru.hyst329.helengine

import ru.hyst329.helengine.Global._

case class Move(
    from: Square,
    to: Square,
    captures: Piece,
    oldEnPassant: Square,
    oldCastling: CastlingFlags
)
