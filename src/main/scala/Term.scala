import scala.collection.immutable.HashMap

trait Term {
  def getType(context: Context): Type
  override def toString(): String
}

object Term {
  final case class True() extends Term {
    def getType(context: Context): Type = new Type.Bool
    override def toString: String = "true"
  }

  final case class False() extends Term {
    def getType(context: Context): Type = new Type.Bool
    override def toString: String = "false"
  }

  final case class Int(value: scala.Int) extends Term {
    def getType(context: Context): Type = new Type.Int
    override def toString: String = value.toString()
  }

  final case class Record(entries: HashMap[String, Term]) extends Term {
    def getType(context: Context): Type =
      new Type.Record(
        entries map (entry => (entry._1, entry._2.getType(context)))
      )
    override def toString: String = "{ " + (for ((name, value) <- entries)
      yield name + ": " + value.toString()).reduce(_ + _) + " }"
  }

  final case class If(test: Term, consequent: Term, alternate: Term)
      extends Term {
    def getType(context: Context): Type = test.getType(context) match {
      case Type.Bool() => {
        val consequentType = consequent.getType(context)
        val alternateType = alternate.getType(context)
        if (
          consequentType.isSubTypeOf(alternateType) && alternateType
            .isSubTypeOf(consequentType)
        ) {
          consequentType
        } else {
          throw new MyException("branches must be in the same type")
        }
      }
      case _ => throw new MyException("test must evaluate to a boolean")
    }
    override def toString: String =
      "if " + test.toString + " then " + consequent.toString + " else " +
        alternate.toString
  }

  final case class Project(target: Term, field: String) extends Term {
    def getType(context: Context): Type = target.getType(context) match {
      case Type.Record(entries) =>
        entries.getOrElse(field, throw new MyException("invalid field"))
      case _ => throw new MyException("only record types are projectable")
    }
    override def toString: String = target match {
      case target @ If(_, _, _) => "(" + target.toString + ")" + "." + field
      case _                    => target.toString + "." + field
    }
  }

  final case class Variable(name: String) extends Term {
    def getType(context: Context): Type = context.getType(name)
    override def toString: String = name
  }

  final case class Abstract(parameter: String, parameterType: Type, body: Term)
      extends Term {
    def getType(context: Context): Type =
      Type.Function(parameterType, body.getType(context))
    override def toString: String =
      "(" + parameter + ": " + parameterType.display + ") => " + body.toString
  }

  final case class Apply(callee: Term, argument: Term) extends Term {
    def getType(context: Context): Type = callee.getType(context) match {
      case Type.Function(parameterType, returnType) => {
        val argumentType = argument.getType(context)
        if (argumentType.isSubTypeOf(parameterType)) {
          returnType
        } else {
          throw new MyException("argument type mismatch")
        }
      }
      case _ => throw new MyException("callee is not a function")
    }
    override def toString: String = (callee match {
      case callee @ If(_, _, _) => "(" + callee.toString + ")"
      case _                    => callee.toString
    }) + "(" + argument.toString + ")"
  }
}
