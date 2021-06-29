// import fastparse._, NoWhitespace._
import scala.io.StdIn.readLine
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

object Main extends App {
  def repl: Unit = while (true) {
    val command = readLine("> ")
    command match {
      case "quit"  => return
      case "exit"  => return
      case command => Parser.parse(command) match {
        case Success(value, _) => {
          println(s"Re-serialized term: $value")
        }
        case Failure(_, index, extra) => {
          val message = extra.trace().longAggregateMsg
          println(s"Syntax error at $index: $message")
        }
      }
    }
  }

  println("Welcome to TAPL Scala!")
  repl
}
