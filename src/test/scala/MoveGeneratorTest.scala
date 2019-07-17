import org.scalatest.{FlatSpec, Matchers, OptionValues}
import ru.hyst329.helengine.Global._
import ru.hyst329.helengine.{Board, MagicBitBoards, MoveGenerator}

class MoveGeneratorTest extends FlatSpec with Matchers with OptionValues {
  "Move generator" should "generate valid pseudo-legal moves" in {
    val board: Board =
      Board.fromFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1").get
    val moves = MoveGenerator.generatePseudoLegal(board)
    moves.length shouldBe 20
  }
}
