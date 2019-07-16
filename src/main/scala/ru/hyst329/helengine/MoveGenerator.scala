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
    AllSquares.foreach { square =>
      board.getPiece(square.toByte) match {
        case `pawn` =>
        // Generate pawn moves
        // TODO: Pawn moves
        case `knight` =>
          val mask = MagicBitBoards.KingPatterns(square) & ~occupationCurrent
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `bishop` =>
          // Generate bishop moves
          val mask: BitBoard = MagicBitBoards.BishopAttackTable(square)((
            ((occupationAll | MagicBitBoards.BishopMasks(square))
              * MagicBitBoards.BishopMagic(square)) >>> (64 - MagicBitBoards.BishopBits(square))).toInt)
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `rook` =>
          // Generate rook moves
          val mask: BitBoard = MagicBitBoards.RookAttackTable(square)((
            ((occupationAll | MagicBitBoards.RookMasks(square))
              * MagicBitBoards.RookMagic(square)) >>> (64 - MagicBitBoards.RookBits(square))).toInt)
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `queen` =>
          val maskBishop: BitBoard = MagicBitBoards.BishopAttackTable(square)((
            ((occupationAll | MagicBitBoards.BishopMasks(square))
              * MagicBitBoards.BishopMagic(square)) >>> (64 - MagicBitBoards.BishopBits(square))).toInt)
          val maskRook: BitBoard = MagicBitBoards.RookAttackTable(square)((
            ((occupationAll | MagicBitBoards.RookMasks(square))
              * MagicBitBoards.RookMagic(square)) >>> (64 - MagicBitBoards.RookBits(square))).toInt)
          val mask = maskBishop | maskRook
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        // Generate queen moves
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
    result
  }
}
