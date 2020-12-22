package cs.edu.uic.interpreter


trait Expression {}

class IntConstant(val c: Int) extends Expression {}

class BooleanConstant(val c: Boolean) extends Expression {}

class BinaryOperationExpression(val operator: Operator.op, val left: Expression, val right: Expression) extends Expression {}

class IfExpression(val condition: Expression, val thenSide: Expression, val elseSide: Expression) extends Expression {}

class ComparisonExpression(val typ: Type.typ, val left: Expression, val right: Expression) extends Expression {}

class VariableExpression(val variable: Name) extends Expression {}

class LetExpression(val variable: Name, val value: Expression, val body: Expression) extends Expression {}

class FunctionCallExpression(val functionName: Name, val actualArguments: Expression*) extends Expression {}

