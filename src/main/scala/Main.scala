object Main {

  // Constants
  // Input: 474
  // Output: IntValue{v=474}
  val p1 = new IntConstant(474)

  // Constants, BinaryOperation
  // Input: ( 400 + 74 ) / 3
  // Output: IntValue{v=158}
  val p2 = new BinaryOperationExpression(
    Operator.DIV,
    new BinaryOperationExpression(
      Operator.PLUS,
      new IntConstant(400),
      new IntConstant(74)
    ),
    new IntConstant(3)
  )

  // Constants, BinaryOperation, Comparison
  // Input: ( 400 + 74 ) / 3 == 158
  // Output: BooleanValue{v=true}
  val p3 = new ComparisonExpression(
    Type.EQ,
    new BinaryOperationExpression(
      Operator.DIV,
      new BinaryOperationExpression(
        Operator.PLUS,
        new IntConstant(400),
        new IntConstant(74)
      ),
      new IntConstant(3)
    ),
    new IntConstant(158)
  )

  // Constants, BinaryOperation, Comparison, Conditional Execution
  // Input: if (((400 + 74) / 3) == 158)
  //        then 474
  //        else 474/0
  // Output: IntValue{v=474}
  val p4 = new IfExpression(
    new ComparisonExpression(
      Type.EQ,
      new BinaryOperationExpression(
        Operator.DIV,
        new BinaryOperationExpression(
          Operator.PLUS,
          new IntConstant(400),
          new IntConstant(74)
        ),
        new IntConstant(3)
      ),
      new IntConstant(158)
    ),
    new IntConstant(474),
    new BinaryOperationExpression(
      Operator.DIV,
      new IntConstant(474),
      new IntConstant(0)
    )
  )

  // Constants, BinaryOperation, Comparison, Conditional Execution, Variable
  // Input: let bot = 3 in
  //          (let bot = 2 in bot)
  //          +
  //          (if (bot == 0) then 474/0 else (400+74)/bot)
  // Output: IntValue{v=160} or IntValue{v=239}
  val p5 = new LetExpression(
    new Name("bot"),
    new IntConstant(3),
    new BinaryOperationExpression(
      Operator.PLUS,
      new LetExpression(
        new Name("bot"),
        new IntConstant(2),
        new VariableExpression(new Name("bot"))
      ),
      new IfExpression(
        new ComparisonExpression(
          Type.EQ,
          new VariableExpression(new Name("bot")),
          new IntConstant(0)
        ),
        new BinaryOperationExpression(
          Operator.DIV,
          new IntConstant(474),
          new IntConstant(0)
        ),
        new BinaryOperationExpression(
          Operator.DIV,
          new BinaryOperationExpression(
            Operator.PLUS,
            new IntConstant(400),
            new IntConstant(74)
          ),
          new VariableExpression(new Name("bot"))
        )
      )
    )
  )


  //function f(top,bot) :
  //  if (bot == 0) then 0 else top/bot
  val f = new Function(
    new Name("f"),
    new IfExpression(
      new ComparisonExpression(
        Type.EQ,
        new VariableExpression(new Name("bot")),
        new IntConstant(0)
      ),
      new IntConstant(0),
      new BinaryOperationExpression(
        Operator.DIV,
        new VariableExpression(new Name("top")),
        new VariableExpression(new Name("bot")),
      )
    ),
    new Name("top"),
    new Name("bot")
  )

  // Constants, BinaryOperation, Comparison, Conditional Execution, Variable, Functions
  // Input: function f(top,bot) :
  //          if (bot == 0) then 0 else top/bot
  //
  //        let bot = 3 in
  //          (let bot = 2 in bot)
  //          +
  //          (f(400+74,bot) + f(470+4,0))
  // Output: IntValue{v=160} or IntValue{v=239}
  val p6 = new LetExpression(
    new Name("bot"),
    new IntConstant(3),
    new BinaryOperationExpression(
      Operator.PLUS,
      new LetExpression(
        new Name("bot"),
        new IntConstant(2),
        new VariableExpression(new Name("bot"))
      ),
      new BinaryOperationExpression(
        Operator.PLUS,
        new FunctionCallExpression(
          new Name("f"),
          new BinaryOperationExpression(
            Operator.PLUS,
            new IntConstant(400),
            new IntConstant(74)
          ),
          new VariableExpression(new Name("bot"))
        ),
        new FunctionCallExpression(
          new Name("f"),
          new BinaryOperationExpression(
            Operator.PLUS,
            new IntConstant(400),
            new IntConstant(74)
          ),
          new IntConstant(0)
        )
      )
    )
  )


  def main(args: Array[String]): Unit = {
    val env = new LexicalScopedEnvironment
    val functions = List(f)

    println("Result of P1 : " + evaluate(p1, env, functions))
    println("Result of P2 : " + evaluate(p2, env, functions))
    println("Result of P3 : " + evaluate(p3, env, functions))
    println("Result of P4 : " + evaluate(p4, env, functions))
    println("Result of P5 : " + evaluate(p5, env, functions))
    println("Result of P6 : " + evaluate(p6, env, functions))
  }


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
        val f = findFunction(call.functionName, functions)

        var evaluationEnvironment = env

        for (i <- 0 until f.formalNamesOfArguments.length) {
          val actualArgumentValue = evaluate(call.actualArguments(i), env, functions)
          evaluationEnvironment = evaluationEnvironment.bind(f.formalNamesOfArguments(i), actualArgumentValue)
        }
        evaluate(f.body, evaluationEnvironment, functions)

      case _ => throw new Error("Unknown Expression: " + exp.getClass.getSimpleName)
    }
  }


  object Operator extends Enumeration {
    type op = Value
    val PLUS, MINUS, TIMES, DIV: Operator.Value = Value
  }


  object Type extends Enumeration {
    type typ = Value
    val EQ: Type.Value = Value
  }


  trait Value {}

  class IntValue(val v: Int) extends Value {
    override def toString: String = "IntValue{" + "v=" + v + '}'
  }

  class BooleanValue(val b: Boolean) extends Value {
    override def toString: String = "BooleanValue{" + "v=" + b + '}'
  }

  class Name(val theName: String) {
    override def toString: String = "Name{" + theName + '}'

    override def equals(obj: Any): Boolean = {
      if (obj == null || this.getClass != obj.getClass) return false
      val name = obj.asInstanceOf[Name]
      theName.equals(name.theName)
    }
  }


  trait Expression {}

  class IntConstant(val c: Int) extends Expression {}

  class BooleanConstant(val c: Boolean) extends Expression {}

  class BinaryOperationExpression(val operator: Operator.op, val left: Expression, val right: Expression) extends Expression {}

  class IfExpression(val condition: Expression, val thenSide: Expression, val elseSide: Expression) extends Expression {}

  class ComparisonExpression(val typ: Type.typ, val left: Expression, val right: Expression) extends Expression {}

  class VariableExpression(val variable: Name) extends Expression {}

  class LetExpression(val variable: Name, val value: Expression, val body: Expression) extends Expression {}

  class FunctionCallExpression(val functionName: Name, val actualArguments: Expression*) extends Expression {}


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


  class Binding(val name: Name, var value: Value) {
    override def toString: String = "Binding{" + "name=" + name + ", value=" + value + '}'
  }


  class Function(val name: Name, val body: Expression, val formalNamesOfArguments: Name*) {
  }

  def findFunction(name: Name, toSearch: List[Function]): Function = {
    if (toSearch eq Nil) {
      throw new Error("No such function: " + name)
    }

    if (toSearch.head.name.equals(name)) {
      return toSearch.head
    }
    findFunction(name, toSearch.tail)
  }
}




