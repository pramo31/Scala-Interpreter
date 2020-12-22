package cs.edu.uic.interpreter

class Binding(val name: Name, var value: Value) {
  override def toString: String = "Binding{" + "name=" + name + ", value=" + value + '}'
}
