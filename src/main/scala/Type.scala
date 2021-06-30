import scala.collection.immutable.HashMap

trait Type {
  override def toString: String
  def isSubTypeOf(that: Type): Boolean
}

object Type {
  case class Bool() extends Type {
    override def toString: String = "Bool"

    def isSubTypeOf(that: Type): Boolean = that match {
      case Top() => true
      case _     => false
    }
  }

  case class Int() extends Type {
    override def toString: String = "Int"

    def isSubTypeOf(that: Type): Boolean = that match {
      case Top() => true
      case _     => false
    }
  }

  case class Function(paramType: Type, returnType: Type) extends Type {
    override def toString: String = s"$paramType -> $returnType"

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
    override def toString: String = (for ((name, ty) <- entries)
      yield s"$name: $ty").mkString("Record { ", ", ", " }")

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
    override def toString: String = "Top"

    def isSubTypeOf(that: Type): Boolean = that match {
      case Top() => true
      case _     => false
    }
  }
}
