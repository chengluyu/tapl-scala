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
    override def toString: String = value.toString
  }

  final case class Record(entries: HashMap[String, Term]) extends Term {
    def getType(context: Context): Type =
      new Type.Record(
        entries map (entry => (entry._1, entry._2.getType(context)))
      )
    override def toString: String = (for ((name, value) <- entries)
      yield s"$name: $value").mkString("{ ", ", ", " }")
  }

  object If {
    def fromTuple(t: (Term, Term, Term)) = new If(t._1, t._2, t._3)
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
          throw new MyException(
            s"""branches of the if statement must be in the same type
               |  consequent :: $consequentType
               |  alternate  :: $alternateType""".stripMargin
          )
        }
      }
      case testType =>
        throw new MyException(
          s"expect condition of the if statement to be boolean, however received $testType"
        )
    }
    override def toString: String = s"if $test then $consequent else $alternate"
  }

  final case class Project(target: Term, field: String) extends Term {
    def getType(context: Context): Type = target.getType(context) match {
      case Type.Record(entries) =>
        entries.getOrElse(field, throw new MyException("invalid field"))
      case targetType =>
        throw new MyException(
          s"""only record types are projectable,
             |however $target is $targetType""".stripMargin
        )
    }
    override def toString: String = target match {
      case target @ If(_, _, _) => s"($target).$field"
      case _                    => s"$target.$field"
    }
  }

  final case class Variable(name: String) extends Term {
    def getType(context: Context): Type = context.getType(name)
    override def toString: String = name
  }

  object Abstract {
    def fromTuple(t: (String, Type, Term)) = new Abstract(t._1, t._2, t._3)
  }

  final case class Abstract(parameter: String, parameterType: Type, body: Term)
      extends Term {
    def getType(context: Context): Type =
      Type.Function(
        parameterType,
        body.getType(context.add(parameter, parameterType))
      )
    override def toString: String = s"($parameter: $parameterType) => $body"
  }

  final case class Apply(callee: Term, argument: Term) extends Term {
    def getType(context: Context): Type = callee.getType(context) match {
      case Type.Function(parameterType, returnType) => {
        val argumentType = argument.getType(context)
        if (argumentType.isSubTypeOf(parameterType)) {
          returnType
        } else {
          throw new MyException(s"""argument type mismatch
                                   |  expect $parameterType
                                   |  received $argumentType""".stripMargin)
        }
      }
      case calleeType =>
        throw new MyException(s"""callee is not a function
                                 |however received $calleeType""".stripMargin)
    }
    override def toString: String = (callee match {
      case callee @ If(_, _, _) => s"($callee)"
      case _                    => callee.toString
    }) + s"($argument)"
  }
}
