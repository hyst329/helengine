package ru.hyst329.helengine

import ru.hyst329.helengine.Global._

import scala.collection.mutable.ArrayBuffer

case class Board(
    var bitBoards: Array[BitBoard],
    var enPassantSquare: Square,
    var castlingFlags: CastlingFlags,
    var moveNumber: Int,
    var halfmoveCounter: Int,
    var whiteToMove: Boolean
) {

  // Occupied squares bit boards
  @inline def occupationWhite: BitBoard =
    bitBoards(WhitePawn) | bitBoards(WhiteKnight) | bitBoards(WhiteBishop) |
      bitBoards(WhiteRook) | bitBoards(WhiteQueen) | bitBoards(WhiteKing)

  @inline def occupationBlack: BitBoard =
    bitBoards(BlackPawn) | bitBoards(BlackKnight) | bitBoards(BlackBishop) |
      bitBoards(BlackRook) | bitBoards(BlackQueen) | bitBoards(BlackKing)

  @inline def occupationAll: BitBoard = ~bitBoards(Empty)

  @inline def occupationCurrentSide: BitBoard  = if (whiteToMove) occupationWhite else occupationBlack
  @inline def occupationOppositeSide: BitBoard = if (whiteToMove) occupationBlack else occupationWhite

  @inline def getPiece(square: Square): Piece = {
    val mask = 1L << square
    (Empty.toInt to BlackKing).foreach { piece =>
      if ((bitBoards(piece) & mask) != 0) return piece.toByte
    }
    Empty
  }

  val moves: ArrayBuffer[Move] = ArrayBuffer.empty
  var hash: Hash = 0L
  // Hash init block
  AllSquares.foreach { square =>
    hash ^= MagicBitBoards.ZobristTable(square)(getPiece(square.toByte))
  }

  def makeMove(move: Move): Unit = {
    bitBoards(move.movingPiece) &= ~(1L << move.from)
    bitBoards(Empty) |= (1L << move.from)
    bitBoards(if(move.promotesTo != Empty) move.promotesTo else move.movingPiece) |= (1L << move.to)
    bitBoards(move.captures) &= ~(1L << move.to)
    hash ^= MagicBitBoards.ZobristTable(move.from)(move.movingPiece)
    hash ^= MagicBitBoards.ZobristTable(move.to)(move.captures)
    hash ^= MagicBitBoards.ZobristTable(move.to)(move.movingPiece)
    hash = -hash
    if (move.castlingToPlain.isDefined) {
      // castling, so we need to move the rook, not only the king
      move.to match {
        case G1 => // white castles king-side
          bitBoards(WhiteRook) &= ~(1L << H1)
          bitBoards(WhiteRook) |= ~(1L << F1)
          bitBoards(Empty) |=  (1L << H1)
          bitBoards(Empty) &= ~(1L << F1)
          hash ^= MagicBitBoards.ZobristTable(H1)(WhiteRook)
          hash ^= MagicBitBoards.ZobristTable(F1)(WhiteRook)
        case C1 => // white castles queen-side
          bitBoards(WhiteRook) &= ~(1L << A1)
          bitBoards(WhiteRook) |=  (1L << D1)
          bitBoards(Empty) |=  (1L << A1)
          bitBoards(Empty) &= ~(1L << D1)
          hash ^= MagicBitBoards.ZobristTable(A1)(WhiteRook)
          hash ^= MagicBitBoards.ZobristTable(D1)(WhiteRook)
        case G8 => // black castles king-side
          bitBoards(BlackRook) &= ~(1L << H8)
          bitBoards(BlackRook) |=  (1L << F8)
          bitBoards(Empty) |=  (1L << H8)
          bitBoards(Empty) &= ~(1L << F8)
          hash ^= MagicBitBoards.ZobristTable(H8)(BlackRook)
          hash ^= MagicBitBoards.ZobristTable(F8)(BlackRook)
        case C8 => // black castles queen-side
          bitBoards(BlackRook) &= ~(1L << A8)
          bitBoards(BlackRook) |=  (1L << D8)
          bitBoards(Empty) |=  (1L << A8)
          bitBoards(Empty) &= ~(1L << D8)
          hash ^= MagicBitBoards.ZobristTable(A8)(BlackRook)
          hash ^= MagicBitBoards.ZobristTable(D8)(BlackRook)
        case _ =>
      }
    }
    // If it's a king move, forfeit any castling rights for that side
    if (move.movingPiece == WhiteKing) {
      castlingFlags &= ~(WhiteKingSide | WhiteQueenSide)
    }
    if (move.movingPiece == BlackKing) {
      castlingFlags &= ~(BlackKingSide | BlackQueenSide)
    }
    // If it's a rook move from corner, forfeit any castling rights for that rook
    if (move.movingPiece == WhiteRook && move.from == H1) {
      castlingFlags &= ~WhiteKingSide
    }
    if (move.movingPiece == WhiteRook && move.from == A1) {
      castlingFlags &= ~WhiteQueenSide
    }
    if (move.movingPiece == BlackRook && move.from == H8) {
      castlingFlags &= ~BlackKingSide
    }
    if (move.movingPiece == BlackRook && move.from == A8) {
      castlingFlags &= ~BlackQueenSide
    }
    switchSides()
    moves += move
    if ((move.movingPiece == WhitePawn && move.to - move.from == 16) || (move.movingPiece == BlackPawn && move.from - move.to == 16))
      enPassantSquare = ((move.from + move.to) >>> 1).toByte
    else
      enPassantSquare = InvalidSquare
    if (whiteToMove) moveNumber += 1
    if (move.movingPiece != BlackPawn && move.movingPiece != WhitePawn && move.captures == Empty)
      halfmoveCounter += 1
    else
      halfmoveCounter = 0
    // Probably all, but still needs testing
  }

  def unmakeMove(): Move = {
    val move = moves.last
    moves.remove(moves.size - 1)
    bitBoards(move.movingPiece) |= (1L << move.from)
    bitBoards(Empty) &= ~(1L << move.from)
    bitBoards(if(move.promotesTo != Empty) move.promotesTo else move.movingPiece) &= ~(1L << move.to)
    bitBoards(move.captures) |= (1L << move.to)
    hash ^= MagicBitBoards.ZobristTable(move.from)(move.movingPiece)
    hash ^= MagicBitBoards.ZobristTable(move.to)(move.captures)
    hash ^= MagicBitBoards.ZobristTable(move.to)(move.movingPiece)
    hash = -hash
    // TODO: Castling move rook back
    if (move.castlingToPlain.isDefined) {
      // castling, so we need to move the rook, not only the king
      move.to match {
        case G1 => // white castles king-side
          bitBoards(WhiteRook) |=  (1L << H1)
          bitBoards(WhiteRook) &= ~(1L << F1)
          bitBoards(Empty) &= ~(1L << H1)
          bitBoards(Empty) |=  (1L << F1)
          hash ^= MagicBitBoards.ZobristTable(H1)(WhiteRook)
          hash ^= MagicBitBoards.ZobristTable(F1)(WhiteRook)
        case C1 => // white castles queen-side
          bitBoards(WhiteRook) |=  (1L << A1)
          bitBoards(WhiteRook) &= ~(1L << D1)
          bitBoards(Empty) &= ~(1L << A1)
          bitBoards(Empty) |=  (1L << D1)
          hash ^= MagicBitBoards.ZobristTable(A1)(WhiteRook)
          hash ^= MagicBitBoards.ZobristTable(D1)(WhiteRook)
        case G8 => // black castles king-side
          bitBoards(BlackRook) |=  (1L << H8)
          bitBoards(BlackRook) &= ~(1L << F8)
          bitBoards(Empty) &= ~(1L << H8)
          bitBoards(Empty) |=  (1L << F8)
          hash ^= MagicBitBoards.ZobristTable(H8)(BlackRook)
          hash ^= MagicBitBoards.ZobristTable(F8)(BlackRook)
        case C8 => // black castles queen-side
          bitBoards(BlackRook) |=  (1L << A8)
          bitBoards(BlackRook) &= ~(1L << D8)
          bitBoards(Empty) &= ~(1L << A8)
          bitBoards(Empty) |=  (1L << D8)
          hash ^= MagicBitBoards.ZobristTable(A8)(BlackRook)
          hash ^= MagicBitBoards.ZobristTable(D8)(BlackRook)
        case _ =>
      }
    }
    // Old flags
    switchSides()
    enPassantSquare = move.oldEnPassant
    castlingFlags = move.oldCastling
    halfmoveCounter = move.oldCastling
    if (!whiteToMove) moveNumber -= 1
    move
  }

  @inline def switchSides(): Unit = this.whiteToMove = !this.whiteToMove

  override def toString: String = {
    (7 to 0 by -1).map { rank =>
      (0 to 7).map { file =>
        Global.PieceLetters(getPiece(rank * 8 + file))
      }.mkString("")
    }.mkString("/")
      .replace("________", "8")
      .replace("_______", "7")
      .replace("______", "6")
      .replace("_____", "5")
      .replace("____", "4")
      .replace("___", "3")
      .replace("__", "2")
      .replace("_", "1")
  }
}

object Board {
  def fromFEN(fen: String): Option[Board] = {
    new FENParser(fen).position.run().toOption
  }
}
