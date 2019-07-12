package ru.hyst329.helengine
import scala.io.StdIn.readLine

object ShouldQuitException extends Exception

object MainApp extends App {
  try {
    while (true) {
      val command = readLine
      command match {
        case "uci" => {
          println("id name Helengine v0.0.0")
          println("id author trolley813")
          println("%016x".format(MagicBitBoards.BishopAttackTable(1)(0)))
          println("uciok")
        }
        case "quit" => throw ShouldQuitException
        case null   => throw ShouldQuitException
        case _      => println(s"Unknown command: $command")
      }
    }
  } catch {
    case ShouldQuitException =>
  }
}
