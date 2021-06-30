import scala.collection.immutable.{ArraySeq, HashMap}

class Binding(val termType: Type)

class Context(val bindings: ArraySeq[(String, Binding)] = ArraySeq.empty) {

  def getType(name: String): Type = {
    bindings.find { _._1 == name } match {
      case Some(value) => value._2.termType
      case None =>
        throw new MyException(s"cannot find the binding with name $name")
    }
  }

  def add(name: String, termType: Type) =
    new Context(bindings.prepended((name, new Binding(termType))))

  def remove(name: String) = (bindings.find { _._1 == name }) match {
    case Some(binding) => new Context(bindings.filter { _ != binding })
    case None          => this
  }
}
