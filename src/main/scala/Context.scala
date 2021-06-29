import scala.collection.immutable.{ArraySeq, HashMap}

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
