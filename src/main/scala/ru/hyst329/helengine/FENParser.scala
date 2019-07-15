package ru.hyst329.helengine

import org.parboiled2._
import ru.hyst329.helengine.Global._

class FENParser(val input: ParserInput) extends Parser {
  def fen: Rule1[Board] = rule {
    position ~ EOI
  }

  def position: Rule1[Board] = rule {
    piecePlacement ~ ws ~ activeColor ~ ws ~ castling ~ ws ~ enPassant ~ ws ~ halfmoveCounter ~ ws ~ moveNumber ~> (
        (
            pieces: Array[BitBoard],
            whiteToMove: Boolean,
            castlingFlags: CastlingFlags,
            enPassantSquare: Square,
            halfmoves: Int,
            moves: Int
        ) => {
          Board(pieces, enPassantSquare, castlingFlags, moves, halfmoves, whiteToMove)
        }
    )
  }

  def piecePlacement: Rule1[Array[BitBoard]] = rule {
    8.times(rankString).separatedBy("/") ~> ((ranks: Seq[Array[Piece]]) => {
      val boards = Array.fill[BitBoard](PieceCount)(0x0l)
      (7 to 0 by -1).zip(ranks).foreach {
        case (rank: Int, files: Array[Piece]) =>
          (0 to 7).zip(files).foreach {
            case (file: Int, piece: Piece) =>
              boards(piece) |= 1l << (rank * 8 + file)
          }
      }
      boards
    })
  }

  def rankString: Rule1[Array[Piece]] = rule {
    capture(oneOrMore(anyOf("12345678KQRBNPkqrbnp"))) ~> ((s: String) => {
      val pieces: Array[Piece] = Array.fill(8)(0.toByte)
      var currentIndex         = 0
      s.foreach {
        case n if '1' to '8' contains n => currentIndex += n.toString.toInt
        case 'K' =>
          pieces(currentIndex) = WhiteKing
          currentIndex += 1
        case 'Q' =>
          pieces(currentIndex) = WhiteQueen
          currentIndex += 1
        case 'R' =>
          pieces(currentIndex) = WhiteRook
          currentIndex += 1
        case 'B' =>
          pieces(currentIndex) = WhiteBishop
          currentIndex += 1
        case 'N' =>
          pieces(currentIndex) = WhiteKnight
          currentIndex += 1
        case 'P' =>
          pieces(currentIndex) = WhitePawn
          currentIndex += 1
        case 'k' =>
          pieces(currentIndex) = BlackKing
          currentIndex += 1
        case 'q' =>
          pieces(currentIndex) = BlackQueen
          currentIndex += 1
        case 'r' =>
          pieces(currentIndex) = BlackRook
          currentIndex += 1
        case 'b' =>
          pieces(currentIndex) = BlackBishop
          currentIndex += 1
        case 'n' =>
          pieces(currentIndex) = BlackKnight
          currentIndex += 1
        case 'p' =>
          pieces(currentIndex) = BlackPawn
          currentIndex += 1
        case _ =>
      }
      pieces
    })
  }

  def activeColor: Rule1[Boolean] = rule {
    (str("w") ~> (() => true)) | (str("b") ~> (() => false))
  }

  def castling: Rule1[CastlingFlags] = rule {
    (str("-") ~> (() => 0.toByte)) | (capture(oneOrMore(anyOf("KQkq"))) ~> ((castling: String) => {
      var flags = 0
      castling.foreach {
        case 'K' => flags |= 1
        case 'Q' => flags |= 2
        case 'k' => flags |= 4
        case 'q' => flags |= 8
        case _   =>
      }
      flags.toByte
    }))
  }

  def enPassant: Rule1[Square] = rule {
    (str("-") ~> (() => 0.toByte)) |
      (capture(anyOf("abcdefgh")) ~> ((file: String) => file.head - 'a') ~
        capture(anyOf("12345678")) ~> ((rank: String) => rank.head - '8')) ~>
        ((file: Int, rank: Int) => (rank * 8 + file).toByte)
  }

  def integer: Rule1[Int] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((s: String) => s.toInt)
  }

  def halfmoveCounter: Rule1[Int] = integer

  def moveNumber: Rule1[Int] = integer

  def ws = rule {
    quiet(zeroOrMore(anyOf(" \t")))
  }
}
