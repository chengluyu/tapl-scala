// import fastparse._, NoWhitespace._
import scala.io.StdIn.readLine
import scala.collection.immutable.{ArraySeq, HashMap}

trait Type {
  def display: String
  def isSubTypeOf(that: Type): Boolean
}

object Type {
  case class Bool() extends Type {
    def display: String = "Bool"
    def isSubTypeOf(that: Type): Boolean = that match {
      case Top() => true
      case _     => false
    }
  }

  case class Int() extends Type {
    def display: String = "Int"
    def isSubTypeOf(that: Type): Boolean = that match {
      case Top() => true
      case _     => false
    }
  }

  case class Function(paramType: Type, returnType: Type) extends Type {
    def display: String = paramType.display + " -> " + returnType.display
    def isSubTypeOf(that: Type): Boolean = that match {
      case Top() => true
      case Function(paramType, returnType) =>
        this.paramType.isSubTypeOf(paramType) && this.returnType.isSubTypeOf(
          paramType
        )
      case _ => false
    }
  }

  case class Record(entries: HashMap[String, Type]) extends Type {
    def display: String = "Record { " + (for ((name, ty) <- entries)
      yield name + ": " + ty.display) + " }"
    def isSubTypeOf(that: Type): Boolean = that match {
      case Top() => true
      case Record(entries) =>
        entries forall (entry => {
          this.entries.get(entry._1) match {
            case Some(value) => value.isSubTypeOf(entry._2)
            case None        => false
          }
        })
      case _ => false
    }
  }

  case class Top() extends Type {
    def display: String = "Top"
    def isSubTypeOf(that: Type): Boolean = that match {
      case Top() => true
      case _     => false
    }
  }
}

trait Term {
  def getType(context: Context): Type
}

object Term {
  final case class True() extends Term {
    def getType(context: Context): Type = new Type.Bool
  }
  final case class False() extends Term {
    def getType(context: Context): Type = new Type.Bool
  }
  final case class Int(value: scala.Int) extends Term {
    def getType(context: Context): Type = new Type.Int
  }
  final case class Record(entries: HashMap[String, Term]) extends Term {
    def getType(context: Context): Type =
      new Type.Record(
        entries map (entry => (entry._1, entry._2.getType(context)))
      )
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
  }
  final case class Project(target: Term, field: String) extends Term {
    def getType(context: Context): Type = target.getType(context) match {
      case Type.Record(entries) =>
        entries.getOrElse(field, throw new MyException("invalid field"))
      case _ => throw new MyException("only record types are projectable")
    }
  }
  final case class Variable(name: String) extends Term {
    def getType(context: Context): Type = context.getType(name)
  }
  final case class Abstract(parameter: String, parameterType: Type, body: Term)
      extends Term {
    def getType(context: Context): Type =
      Type.Function(parameterType, body.getType(context))
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
  }
}

class Binding(val termType: Type)

class Context {
  var bindings = ArraySeq.empty[(String, Binding)]

  def getType(name: String): Type = {
    bindings.find { _._1 == name } match {
      case Some(value) => value._2.termType
      case None        => throw new MyException("cannot find the binding")
    }
  }

  def add(name: String, termType: Type) = {
    bindings = bindings.prepended((name, new Binding(termType)))
  }

  def remove(name: String) = {
    for (binding <- bindings.find { _._1 == name }) {
      bindings = bindings.filter { _ != binding }
    }
  }
}

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

class MyException(message: String) extends Exception(message) {

  def this(message: String, cause: Throwable) {
    this(message)
    initCause(cause)
  }

  def this(cause: Throwable) {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() {
    this(null: String)
  }
}
