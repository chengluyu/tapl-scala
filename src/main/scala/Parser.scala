import fastparse._, SingleLineWhitespace._
import scala.collection.immutable.HashMap

object Parser {
  def parse(x: String) = fastparse.parse(x, TermParser.root(_))

  private def ident[_: P] = CharIn("a-zA-Z").repX(1).!
  private def digits[_: P] = CharIn("0-9").repX(1).!

  private object TermParser {
    def root[_: P]: P[Term] = P(term ~ End)
    def term[_: P]: P[Term] = P(complex | simpleWithTails)
    def simple[_: P]: P[Term] = P(yea | nay | int | variable | record | paren)
    def simpleWithTails[_: P]: P[Term] = P(
      (simple ~/ tail.rep).map({ case (a, b) =>
        b.foldLeft(a)((target, postfix) =>
          postfix match {
            case Left(argument) => Term.Apply(target, argument)
            case Right(name)    => Term.Project(target, name)
          }
        )
      })
    )
    def complex[_: P]: P[Term] = P(condition | abstraction)
    def applyTail[_: P] = P(("(" ~/ term.map(Left(_)) ~ ")"))
    def projectTail[_: P] = P("." ~/ ident.map(Right(_)))
    def tail[_: P]: P[Either[Term, String]] = P(applyTail | projectTail)
    def paren[_: P]: P[Term] = P("(" ~ term ~ ")")
    def yea[_: P]: P[Term.True] = P("true").map(_ => Term.True())
    def nay[_: P]: P[Term.False] = P("false").map(_ => Term.False())
    def int[_: P]: P[Term.Int] = P(digits.map(_.toInt) map Term.Int)
    def condition[_: P]: P[Term.If] = P(
      ("if" ~/ term ~/ "then" ~/ term ~/ "else" ~/ term) map Term.If.fromTuple
    )
    def abstraction[_: P]: P[Term.Abstract] = P(
      ("(" ~ ident ~ ":" ~ TypeParser.root ~ ")" ~ "=>" ~ term)
        map Term.Abstract.fromTuple
    )
    def variable[_: P]: P[Term.Variable] = P(ident.map(Term.Variable))
    def record[_: P]: P[Term.Record] = P(
      "{" ~/ (ident ~/ ":" ~/ term)
        .rep(sep = ",")
        .map(HashMap.from)
        .map(Term.Record) ~ "}"
    )
  }

  private object TypeParser {
    def root[_: P]: P[Type] = P(
      atom.rep(1, sep = "->").map(_.reduceLeft(Type.Function))
    )
    def atom[_: P]: P[Type] = P(bool | int | top | paren | record)
    def bool[_: P]: P[Type] = P("Bool").map(_ => Type.Bool())
    def int[_: P]: P[Type] = P("Int").map(_ => Type.Int())
    def top[_: P]: P[Type] = P("Any").map(_ => Type.Top())
    def paren[_: P]: P[Type] = P("(" ~/ root ~ ")")
    def record[_: P]: P[Type] = P(
      "{" ~/ entry.rep(sep = ",").map(HashMap.from).map(Type.Record) ~ "}"
    )
    def entry[_: P]: P[(String, Type)] = P(ident ~/ ":" ~ root)
  }
}
