package ru.hyst329.helengine

import ru.hyst329.helengine.Global._

object MoveGenerator {
  def generatePseudoLegal(board: Board): List[Move] = {
    var result: List[Move] = List()
    val (pawn, knight, rook, bishop, queen, king) = if (board.whiteToMove) {
      (WhitePawn, WhiteKnight, WhiteRook, WhiteBishop, WhiteQueen, WhiteKing)
    } else {
      (BlackPawn, BlackKnight, BlackRook, BlackBishop, BlackQueen, BlackKing)
    }
    val occupationCurrent  = board.occupationCurrentSide
    val occupationOpposite = board.occupationOppositeSide
    val occupationAll      = board.occupationAll

    val (pawnMovePatterns, pawnCapturePatterns) = if (board.whiteToMove) {
      (MagicBitBoards.WhitePawnMovePatterns, MagicBitBoards.WhitePawnCapturePatterns)
    } else {
      (MagicBitBoards.BlackPawnMovePatterns, MagicBitBoards.BlackPawnCapturePatterns)
    }
    AllSquares.foreach { square =>
      board.getPiece(square.toByte) match {
        case `pawn` =>
          // Generate pawn moves
          // Pawn moves
          val maskMovesAll = pawnMovePatterns(square) & board.bitBoards(Empty)
          // if the square on the 3rd/6th rank is occupied, no double move (no jumping)
          val maskMoves = if(board.whiteToMove) {
            maskMovesAll & ~((board.occupationAll & 0x0000000000FF0000L) << 8)
          } else {
            maskMovesAll & ~((board.occupationAll & 0x0000FF0000000000L) >> 8)
          }
          // Pawn captures (we consider en-passant square occupied only for this purpose)
          val maskCaptures = if(board.enPassantSquare != InvalidSquare)  {
            pawnCapturePatterns(square) & (occupationOpposite | 1L << board.enPassantSquare)
          } else {
            pawnCapturePatterns(square) & occupationOpposite
          }
          val mask = maskMoves | maskCaptures
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `knight` =>
          // Generate knight moves
          val mask = MagicBitBoards.KnightPatterns(square) & ~occupationCurrent
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `bishop` =>
          // Generate bishop moves
          val mask: BitBoard = MagicBitBoards.BishopAttackTable(square)((
            ((occupationAll & MagicBitBoards.BishopMasks(square))
              * MagicBitBoards.BishopMagic(square)) >>> (64 - MagicBitBoards.BishopBits(square))).toInt) & ~occupationCurrent
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `rook` =>
          // Generate rook moves
          val mask: BitBoard = MagicBitBoards.RookAttackTable(square)((
            ((occupationAll & MagicBitBoards.RookMasks(square))
              * MagicBitBoards.RookMagic(square)) >>> (64 - MagicBitBoards.RookBits(square))).toInt) & ~occupationCurrent
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `queen` =>
          // Generate queen moves
          val maskBishop: BitBoard = MagicBitBoards.BishopAttackTable(square)((
            ((occupationAll & MagicBitBoards.BishopMasks(square))
              * MagicBitBoards.BishopMagic(square)) >>> (64 - MagicBitBoards.BishopBits(square))).toInt)
          val maskRook: BitBoard = MagicBitBoards.RookAttackTable(square)((
            ((occupationAll & MagicBitBoards.RookMasks(square))
              * MagicBitBoards.RookMagic(square)) >>> (64 - MagicBitBoards.RookBits(square))).toInt)
          val mask = (maskBishop | maskRook) & ~occupationCurrent
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `king` =>
          // Generate king moves
          val mask = MagicBitBoards.KingPatterns(square) & ~occupationCurrent
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case _ =>
        // Do nothing, either an opponent's piece or empty square
      }
    }
    // TODO: Castling
    result
  }
}
