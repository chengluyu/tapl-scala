object Printer {
  def print(term: Term): String = print(term, 0).mkString("\n")

  private def print(term: Term, depth: Int): Seq[String] =
    term match {
      case Term.True()     => Seq("True")
      case Term.False()    => Seq("False")
      case Term.Int(value) => Seq(s"Int(value: $value)")
      case Term.Record(entries) =>
        Seq("Record") ++ entries.iterator.flatMap { case (name, value) =>
          print(s"$name: ", value, depth + 1)
        }
      case Term.If(test, consequent, alternate) =>
        Seq("If") ++
          print("test: ", test, depth + 1) ++
          print("consequent: ", consequent, depth + 1) ++
          print("alternate: ", alternate, depth + 1)
      case Term.Project(target, field) =>
        Seq("Project") ++
          print("target: ", target, depth + 1) ++
          Seq(indent(s"field: $field", depth + 1))
      case Term.Variable(name) => Seq(s"Variable(name: $name)")
      case Term.Abstract(parameter, parameterType, body) =>
        Seq("Abstract") ++
          Seq(indent(s"parameter: $parameter", depth + 1)) ++
          Seq(indent(s"parameterType: $parameterType", depth + 1)) ++
          print("body: ", body, depth + 1)
      case Term.Apply(callee, argument) =>
        Seq("Apply") ++
          print("callee: ", callee, depth + 1) ++
          print("argument: ", argument, depth + 1)
    }

  private def print(prefix: String, term: Term, depth: Int): Seq[String] =
    print(term, depth) match {
      case head :: next => indent(prefix + head, depth) :: next
      case Nil          => Seq(indent(prefix, depth))
    }

  private def indent(line: String, depth: Int) = "  " * depth + line
}
