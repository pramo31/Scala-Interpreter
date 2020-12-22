package cs.edu.uic.interpreter

trait Value {}

class IntValue(val v: Int) extends Value {
  override def toString: String = "IntValue{" + "v=" + v + '}'
}

class BooleanValue(val b: Boolean) extends Value {
  override def toString: String = "BooleanValue{" + "v=" + b + '}'
}
