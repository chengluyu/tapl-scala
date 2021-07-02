// import fastparse._, NoWhitespace._
import scala.io.StdIn.readLine
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import scala.util.matching.Regex

object Main extends App {
  val context = new Context()
  val commandPattern = """^:(\w+)(?: (.*))?$""".r("command", "body")

  def perform(command: String, body: String): ReplResult =
    command match {
      case "type" =>
        Parser.parse(body) match {
          case Success(term, _) =>
            try {
              val termType = term.getType(context)
              ReplResult.Success(s"$term :: $termType")
            } catch {
              case e: MyException =>
                ReplResult.Failture(s"Type error: ${e.getMessage}")
            }
          case Failure(_, index, extra) =>
            val message = extra.trace().longAggregateMsg
            ReplResult.Failture(s"Syntax error at $index: $message")
        }
      case "quit" => ReplResult.Termination()
      case "exit" => ReplResult.Termination()
      case "help" => ReplResult.Success("""Available commands:
        | - type <expr>: show type of expr (the default behavior)
        | - quit       : exit the program
        | - exit       : exit the program
        | - help       : show this message""".stripMargin)
    }

  def repl: Unit =
    (readLine("> ") match {
      case line if line.startsWith(":") =>
        commandPattern findFirstMatchIn line match {
          case Some(value) =>
            perform(value.group("command"), value.group("body"))
          case None => ReplResult.Failture(s"unknown command")
        }
      case code => perform("type", code)
    }) match {
      case ReplResult.Termination() =>
        println("Bye.")
      case ReplResult.Success(result) =>
        println(result)
        repl
      case ReplResult.Failture(message) =>
        println(message)
        repl
    }

  println("Welcome to TAPL Scala!")
  repl
}

trait ReplResult {
  override def toString: String
}

object ReplResult {
  case class Termination() extends ReplResult
  case class Success(result: String) extends ReplResult
  case class Failture(message: String) extends ReplResult
}
