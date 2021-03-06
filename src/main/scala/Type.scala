import scala.collection.immutable.HashMap

trait Type {
  override def toString: String
  def :<(that: Type): Boolean
  def ==(other: Type): Boolean
}

object Type {
  case class Bool() extends Type {
    override def toString: String = "Bool"

    def :<(that: Type): Boolean = that match {
      case Top() | Bool() => true
      case _              => false
    }

    def ==(that: Type): Boolean = that match {
      case Bool() => true
      case _      => false
    }
  }

  case class Int() extends Type {
    override def toString: String = "Int"

    def :<(that: Type): Boolean = that match {
      case Top() | Int() => true
      case _             => false
    }

    def ==(that: Type): Boolean = that match {
      case Top() | Int() => true
      case _             => false
    }
  }

  case class Function(paramType: Type, returnType: Type) extends Type {
    override def toString: String = s"$paramType -> $returnType"

    def :<(that: Type): Boolean = that match {
      case Top() => true
      case Function(paramType, returnType) =>
        this.paramType :< paramType && this.returnType :< paramType
      case _ => false
    }

    def ==(that: Type): Boolean = that match {
      case Function(paramType, returnType) =>
        this.paramType == paramType && this.returnType == returnType
      case _ => false
    }
  }

  case class Record(entries: HashMap[String, Type]) extends Type {
    override def toString: String = (for ((name, ty) <- entries)
      yield s"$name: $ty").mkString("Record { ", ", ", " }")

    def :<(that: Type): Boolean = that match {
      case Top() => true
      case Record(entries) =>
        entries forall (entry =>
          this.entries.get(entry._1) match {
            case Some(value) => value :< entry._2
            case None        => false
          }
        )
      case _ => false
    }

    def ==(that: Type): Boolean = that match {
      case Record(entries) =>
        this.entries.size == entries.size && this.entries.forall {
          case (name, value) =>
            entries.get(name).map(_ == value).getOrElse(false)
        }
      case _ => false
    }
  }

  case class Top() extends Type {
    override def toString: String = "Top"

    def :<(that: Type): Boolean = that match {
      case Top() => true
      case _     => false
    }

    def ==(that: Type): Boolean = that match {
      case Top() => true
      case _     => false
    }
  }
}
