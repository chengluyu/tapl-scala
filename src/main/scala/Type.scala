import scala.collection.immutable.HashMap

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
