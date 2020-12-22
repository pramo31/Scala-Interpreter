package cs.edu.uic.interpreter

class Interpreter {

  def evaluate(exp: Expression, env: Environment, functions: List[Function]): Value = {
    exp.getClass.getSimpleName match {
      case "IntConstant" =>
        val cst = exp.asInstanceOf[IntConstant]
        new IntValue(cst.c)
      case "BinaryOperationExpression" =>
        val bcst = exp.asInstanceOf[BinaryOperationExpression]
        val leftVal = evaluate(bcst.left, env, functions).asInstanceOf[IntValue]
        val rightVal = evaluate(bcst.right, env, functions).asInstanceOf[IntValue]
        bcst.operator match {
          case Operator.PLUS => new IntValue(leftVal.v + rightVal.v)
          case Operator.MINUS => new IntValue(leftVal.v - rightVal.v)
          case Operator.TIMES => new IntValue(leftVal.v * rightVal.v)
          case Operator.DIV => new IntValue(leftVal.v / rightVal.v)
          case _ => throw new Error("Unknown Binary Operator: " + bcst.operator);
        }
      case "BooleanConstant" =>
        val cst = exp.asInstanceOf[BooleanConstant]
        new BooleanValue(cst.c)
      case "ComparisonExpression" =>
        val comp = exp.asInstanceOf[ComparisonExpression]
        val left = evaluate(comp.left, env, functions).asInstanceOf[IntValue]
        val right = evaluate(comp.right, env, functions).asInstanceOf[IntValue]
        comp.typ match {
          case Type.EQ => new BooleanValue(left.v == right.v)
          case _ => throw new Error("Unknown Comparison Type: " + comp.typ)
        }
      case "IfExpression" =>
        val ife = exp.asInstanceOf[IfExpression]
        val cond = evaluate(ife.condition, env, functions).asInstanceOf[BooleanValue]
        if (cond.b) evaluate(ife.thenSide, env, functions)
        else evaluate(ife.elseSide, env, functions)
      case "LetExpression" =>
        val let = exp.asInstanceOf[LetExpression]
        val v = evaluate(let.value, env, functions)
        val newEnv = env.bind(let.variable, v)
        evaluate(let.body, newEnv, functions)
      case "VariableExpression" =>
        val varExp = exp.asInstanceOf[VariableExpression]
        env.lookup(varExp.variable)
      case "FunctionCallExpression" =>
        val call = exp.asInstanceOf[FunctionCallExpression]
        val f = Function.findFunction(call.functionName, functions)

        var evaluationEnvironment = env

        for (i <- 0 until f.formalNamesOfArguments.length) {
          val actualArgumentValue = evaluate(call.actualArguments(i), env, functions)
          evaluationEnvironment = evaluationEnvironment.bind(f.formalNamesOfArguments(i), actualArgumentValue)
        }
        evaluate(f.body, evaluationEnvironment, functions)

      case _ => throw new Error("Unknown Expression: " + exp.getClass.getSimpleName)
    }
  }

}
