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
          val maskMoves = if (board.whiteToMove) {
            maskMovesAll & ~((board.occupationAll & 0X0000000000FF0000L) << 8)
          } else {
            maskMovesAll & ~((board.occupationAll & 0X0000FF0000000000L) >> 8)
          }
          // Pawn captures (we consider en-passant square occupied only for this purpose)
          val maskCaptures = if (board.enPassantSquare != InvalidSquare) {
            pawnCapturePatterns(square) & (occupationOpposite | 1L << board.enPassantSquare)
          } else {
            pawnCapturePatterns(square) & occupationOpposite
          }
          val mask = maskMoves | maskCaptures
          // TODO: Pawn promotions
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
          val mask: BitBoard = MagicBitBoards.BishopAttackTable(square)(
            (((occupationAll & MagicBitBoards.BishopMasks(square))
              * MagicBitBoards.BishopMagic(square)) >>> (64 - MagicBitBoards
              .BishopBits(square))).toInt
          ) & ~occupationCurrent
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `rook` =>
          // Generate rook moves
          val mask: BitBoard = MagicBitBoards.RookAttackTable(square)(
            (((occupationAll & MagicBitBoards.RookMasks(square))
              * MagicBitBoards.RookMagic(square)) >>> (64 - MagicBitBoards
              .RookBits(square))).toInt
          ) & ~occupationCurrent
          result ++= AllSquares
            .filter(s => (mask & 1L << s) != 0)
            .map(dest => Move.fromBoardContext(board, square.toByte, dest.toByte))
        case `queen` =>
          // Generate queen moves
          val maskBishop: BitBoard = MagicBitBoards.BishopAttackTable(square)(
            (((occupationAll & MagicBitBoards.BishopMasks(square))
              * MagicBitBoards.BishopMagic(square)) >>> (64 - MagicBitBoards
              .BishopBits(square))).toInt
          )
          val maskRook: BitBoard = MagicBitBoards.RookAttackTable(square)(
            (((occupationAll & MagicBitBoards.RookMasks(square))
              * MagicBitBoards.RookMagic(square)) >>> (64 - MagicBitBoards.RookBits(square))).toInt
          )
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
    // Castling
    if (board.whiteToMove && (board.castlingFlags & WhiteKingSide) != 0 && (board.bitBoards(Empty) & (1L << F1 | 1L << G1)) == (1L << F1 | 1L << G1)) {
      result +:= Move.fromBoardContext(board, E1, G1)
    }
    if (board.whiteToMove && (board.castlingFlags & WhiteQueenSide) != 0 && (board.bitBoards(Empty) & (1L << D1 | 1L << C1 | 1L << B1)) == (1L << D1 | 1L << C1 | 1L << B1)) {
      result +:= Move.fromBoardContext(board, E1, C1)
    }
    if (!board.whiteToMove && (board.castlingFlags & BlackKingSide) != 0 && (board.bitBoards(Empty) & (1L << F8 | 1L << G8)) == (1L << F8 | 1L << G8)) {
      result +:= Move.fromBoardContext(board, E8, G8)
    }
    if (!board.whiteToMove && (board.castlingFlags & BlackQueenSide) != 0 && (board.bitBoards(Empty) & (1L << D8 | 1L << C1 | 1L << B8)) == (1L << D8 | 1L << C1 | 1L << B8)) {
      result +:= Move.fromBoardContext(board, E8, C8)
    }
    result
  }

  def generateLegal(board: Board): List[Move] =
    generatePseudoLegal(board).filter(checkLegality(board, _))

  def checkLegality(board: Board, move: Move): Boolean = {
    val normalisedMove = move.castlingToPlain
    board.makeMove(move)
    board.switchSides()
    var checkBitBoard = detectCheck(board)
    board.switchSides()
    board.unmakeMove()
    normalisedMove match {
      case Some(m) =>
        // Checking for castling out of check
        if (detectCheck(board) != 0) return false
        // Checking for castling through check
        board.makeMove(m)
        board.switchSides()
        checkBitBoard |= detectCheck(board)
        board.switchSides()
        board.unmakeMove()
      case None => // do nothing if it's a plain move, not a castling
    }
    checkBitBoard == 0
  }

  def detectCheck(board: Board): BitBoard = {
    var result = 0L
    val (pawn, knight, rook, bishop, queen, king) = if (!board.whiteToMove) {
      (WhitePawn, WhiteKnight, WhiteRook, WhiteBishop, WhiteQueen, WhiteKing)
    } else {
      (BlackPawn, BlackKnight, BlackRook, BlackBishop, BlackQueen, BlackKing)
    }
    val pawnCapturePatterns =
      if (board.whiteToMove) MagicBitBoards.WhitePawnCapturePatterns
      else MagicBitBoards.BlackPawnCapturePatterns
    val ownKing         = if (board.whiteToMove) WhiteKing else BlackKing
    val ownKingSquare   = java.lang.Long.numberOfTrailingZeros(board.bitBoards(ownKing))
    val enemyKingSquare = java.lang.Long.numberOfTrailingZeros(board.bitBoards(king))
    // Rooks, bishops and queens
    val bishopAttacks: BitBoard = MagicBitBoards.BishopAttackTable(ownKingSquare)(
      (((board.occupationAll & MagicBitBoards.BishopMasks(ownKingSquare)) * MagicBitBoards
        .BishopMagic(ownKingSquare)) >>> (64 - MagicBitBoards.BishopBits(ownKingSquare))).toInt)
    val rookAttacks: BitBoard = MagicBitBoards.RookAttackTable(ownKingSquare)(
      (((board.occupationAll & MagicBitBoards.RookMasks(ownKingSquare)) * MagicBitBoards
        .RookMagic(ownKingSquare)) >>> (64 - MagicBitBoards.RookBits(ownKingSquare))).toInt)
    result |= (bishopAttacks & (board.bitBoards(bishop) | board.bitBoards(queen)))
    result |= (rookAttacks & (board.bitBoards(rook) | board.bitBoards(queen)))
    // Kings
    result |= (MagicBitBoards.KingPatterns(ownKingSquare) & board.bitBoards(king))
    // Knights
    result |= (MagicBitBoards.KnightPatterns(ownKingSquare) & board.bitBoards(knight))
    // Pawns
    result |= (pawnCapturePatterns(ownKingSquare) & board.bitBoards(pawn))
    result
  }

  def perft(board: Board, depth: Int): Long = {
    require(depth >= 0)
    if (depth == 0) {
      return 1L
    }
    var result = 0L
    val moves  = generateLegal(board)
    if (depth == 1) {
      return moves.length
    }
    moves.foreach { move =>
      board.makeMove(move)
      result += perft(board, depth - 1)
      board.unmakeMove()
    }
    result
  }
}
