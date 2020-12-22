package cs.edu.uic.interpreter

object Operator extends Enumeration {
  type op = Value
  val PLUS, MINUS, TIMES, DIV: Operator.Value = Value
}

object Type extends Enumeration {
  type typ = Value
  val EQ: Type.Value = Value
}
