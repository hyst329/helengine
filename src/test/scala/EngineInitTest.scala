import org.scalatest.{FlatSpec, Matchers, OptionValues}
import ru.hyst329.helengine.{Board, MagicBitBoards}
import ru.hyst329.helengine.Global._

class EngineInitTest extends FlatSpec with Matchers with OptionValues {
  "Engine" should "initialise and load resources properly" in {
    // rook and bishop on a1, on empty board
    "%016x".format(MagicBitBoards.RookAttackTable(0)(0)) shouldBe "01010101010101fe"
    "%016x".format(MagicBitBoards.BishopAttackTable(0)(0)) shouldBe "8040201008040200"
    // rook and bishop on b1, on empty board
    "%016x".format(MagicBitBoards.RookAttackTable(1)(0)) shouldBe "02020202020202fd"
    "%016x".format(MagicBitBoards.BishopAttackTable(1)(0)) shouldBe "0080402010080500"
  }

  "Board" should "construct from FEN" in {
    val board: Option[Board] =
      Board.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    board.value.moveNumber shouldBe 1
    "%016x".format(board.value.bitBoards(BlackKing)) shouldBe "1000000000000000"
    "%016x".format(board.value.bitBoards(WhiteKing)) shouldBe "0000000000000010"
    "%016x".format(board.value.occupationWhite) shouldBe "000000000000ffff"
    "%016x".format(board.value.occupationBlack) shouldBe "ffff000000000000"
  }
}
