import org.scalatest.{FlatSpec, Matchers, OptionValues}
import ru.hyst329.helengine.Global._
import ru.hyst329.helengine.{Board, MagicBitBoards, MoveGenerator}

class MoveGeneratorTest extends FlatSpec with Matchers with OptionValues {
  "Move generator" should "generate valid pseudo-legal moves" in {
    val board1: Board =
      Board.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").get
    val moves1 = MoveGenerator.generatePseudoLegal(board1)
    moves1.length shouldBe 20

    val board2: Board =
      Board.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1").get
    val moves2 = MoveGenerator.generatePseudoLegal(board2)
    moves2.length shouldBe 20
  }

  "Perft function" should "give valid values" in {
    val board: Board =
      Board.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").get
    //MoveGenerator.perft(board, 1) shouldBe 20L
    //MoveGenerator.perft(board, 2) shouldBe 400L
    MoveGenerator.perft(board, 3) shouldBe 8902L
    //MoveGenerator.perft(board, 4) shouldBe 176481L
    //MoveGenerator.perft(board, 5) shouldBe 4865609L
    //MoveGenerator.perft(board, 6) shouldBe 119060324L
  }
}
