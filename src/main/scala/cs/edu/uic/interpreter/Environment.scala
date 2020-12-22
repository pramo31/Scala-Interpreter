package cs.edu.uic.interpreter

trait Environment {

  var bindings: List[Binding] = Nil

  def lookup(name: Name): Value = lookup(name, bindings)

  private def lookup(name: Name, search: List[Binding]): Value = {
    if (search eq Nil) throw new Error("Name " + name + " not found in the environment")
    if (search.head.name.equals(name)) return search.head.value
    lookup(name, search.tail)
  }

  def bind(name: Name, value: Value): Environment
}

class LexicalScopedEnvironment private(binding: Option[Binding], nextInScope: Option[LexicalScopedEnvironment]) extends Environment {
  if (binding.isDefined && nextInScope.isDefined) {
    this.bindings = List.concat(List(binding.get), nextInScope.get.bindings)
  }

  def this() {
    this(Option.empty, Option.empty)
  }

  override def bind(name: Name, value: Value): Environment = {
    val b = new Binding(name, value)
    new LexicalScopedEnvironment(Some(b), Some(this))
  }

  override def toString: String = "Environment{" + "bindings=" + bindings + '}'
}


