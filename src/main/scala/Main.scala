// import fastparse._, NoWhitespace._
import scala.io.StdIn.readLine

object Main extends App {
  def repl: Unit = while (true) {
    val command = readLine("> ")
    command match {
      case "quit"  => return
      case "exit"  => return
      case command => println("You typed " + command)
    }
  }

  println("Welcome to TAPL Scala!")
  repl
}
