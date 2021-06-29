import fastparse._, SingleLineWhitespace._
import scala.collection.immutable.HashMap

object Parser {
  def parse(x: String) = fastparse.parse(x, TermParser.term(_))

  private def ident[_: P]: P[String] = CharIn("a-zA-Z").rep(1).!

  private object TermParser {
    def term[_: P]: P[Term] = P(compound ~ End)
    def compound[_: P]: P[Term] = P(
      (atom ~ (apply | project).rep).map({ case (a, b) =>
        b.foldLeft(a)((target, postfix) =>
          postfix match {
            case argument: Term => new Term.Apply(target, argument)
            case name: String   => new Term.Project(target, name)
          }
        )
      })
    )
    def apply[_: P]: P[Term] = P("(" ~/ compound ~ ")")
    def project[_: P]: P[String] = P("." ~/ ident)
    def atom[_: P]: P[Term] = P(
      yea | nay | int | paren | record | condition | abstraction | variable
    )
    def paren[_: P]: P[Term] = P("(" ~ compound ~ ")")
    def yea[_: P]: P[Term.True] = P("true").map(_ => new Term.True())
    def nay[_: P]: P[Term.False] = P("false").map(_ => new Term.False())
    def int[_: P]: P[Term.Int] = P(
      CharIn("0-9").rep(1).!.map(x => new Term.Int(x.toInt))
    )
    def condition[_: P]: P[Term.If] = P(
      ("if" ~/ compound ~ "then" ~ compound ~ "else" ~ compound).map({
        case (a, b, c) =>
          new Term.If(a, b, c)
      })
    )
    def abstraction[_: P]: P[Term.Abstract] = P(
      ("(" ~ ident ~ ":" ~ TypeParser.termType ~ ")" ~ "=>" ~ compound).map({
        case (a, b, c) =>
          new Term.Abstract(a, b, c)
      })
    )
    def variable[_: P]: P[Term.Variable] = P(ident.map(new Term.Variable(_)))

    def record[_: P]: P[Term.Record] = P(
      "{" ~/ (ident ~/ ":" ~/ compound)
        .rep(sep = ",")
        .map(HashMap.from)
        .map(Term.Record) ~ "}"
    )
  }

  private object TypeParser {
    def termType[_: P]: P[Type] = P(
      atom.rep(1, sep = "->").map(_.reduceLeft(Type.Function))
    )
    def atom[_: P]: P[Type] = P(bool | int | top | paren | record)
    def bool[_: P]: P[Type] = P("Bool").map(_ => new Type.Bool())
    def int[_: P]: P[Type] = P("Int").map(_ => new Type.Int())
    def top[_: P]: P[Type] = P("Any").map(_ => new Type.Top())
    def paren[_: P]: P[Type] = P("(" ~/ termType ~ ")")
    def record[_: P]: P[Type] = P(
      "{" ~ entry.rep(sep = ",").map(HashMap.from).map(Type.Record) ~ "}"
    )
    def entry[_: P]: P[(String, Type)] = P(ident ~/ ":" ~ termType)
  }
}
