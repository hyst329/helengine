import org.scalatest.{FlatSpec, Matchers}
import ru.hyst329.helengine.MagicBitBoards

class EngineInitTest extends FlatSpec with Matchers {
  "Engine" should "initialise and load resources properly" in {
    // rook and bishop on a1, on empty board
    "%016x".format(MagicBitBoards.RookAttackTable(0)(0)) shouldBe "01010101010101fe"
    "%016x".format(MagicBitBoards.BishopAttackTable(0)(0)) shouldBe "8040201008040200"
    // rook and bishop on b1, on empty board
    "%016x".format(MagicBitBoards.RookAttackTable(1)(0)) shouldBe "02020202020202fd"
    "%016x".format(MagicBitBoards.BishopAttackTable(1)(0)) shouldBe "0080402010080500"
  }
}
