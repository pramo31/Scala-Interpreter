package cs.edu.uic.interpreter

import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class TestInterpreter extends AnyFunSuite with BeforeAndAfter {

  val interpreter: Interpreter = Main.getInterpreter
  val environment = new LexicalScopedEnvironment()

  //function f(top,bot) :
  //  if (bot == 0) then 0 else top/bot
  var functionName = "f"
  val f = new Function(
    new Name(functionName),
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
  val functions = List(f)

  before {
  }

  test("Test Int Constant Expression") {
    // Constants
    // Input: 474
    // Output: IntValue{v=474}
    val theInt = 474
    val p1 = new IntConstant(theInt)
    val value = interpreter.evaluate(p1, environment, functions).asInstanceOf[IntValue]
    println("Result of P1 : " + value)
    assert(theInt == value.v)
  }

  test("Test Binary Operation Expression") {
    // Constants, BinaryOperation
    // Input: ( 400 + 74 ) / 3
    // Output: IntValue{v=158}
    val sum_one = 400
    val sum_two = 74
    val divisor = 3
    val p2 = new BinaryOperationExpression(
      Operator.DIV,
      new BinaryOperationExpression(
        Operator.PLUS,
        new IntConstant(sum_one),
        new IntConstant(sum_two)
      ),
      new IntConstant(divisor)
    )

    val value = interpreter.evaluate(p2, environment, functions).asInstanceOf[IntValue]
    println("Result of P2 : " + value)
    val expected = (sum_one + sum_two) / divisor
    assert(expected == value.v)
  }

  test("Test Comparison Expression") {
    // Constants, BinaryOperation, Comparison
    // Input: ( 400 + 74 ) / 3 == 158
    // Output: BooleanValue{v=true}
    val sum_one = 400
    val sum_two = 74
    val divisor = 3
    val comparator = 158
    val p3 = new ComparisonExpression(
      Type.EQ,
      new BinaryOperationExpression(
        Operator.DIV,
        new BinaryOperationExpression(
          Operator.PLUS,
          new IntConstant(sum_one),
          new IntConstant(sum_two)
        ),
        new IntConstant(divisor)
      ),
      new IntConstant(comparator)
    )

    val value = interpreter.evaluate(p3, environment, functions).asInstanceOf[BooleanValue]
    println("Result of P3 : " + value)
    val expected = (sum_one + sum_two) / divisor == comparator
    assert(expected == value.b)
  }

  test("Test Conditional Execution Expression") {
    // Constants, BinaryOperation, Comparison, Conditional Execution
    // Input: if (((400 + 74) / 3) == 158)
    //        then 474
    //        else 474/0
    // Output: IntValue{v=474}

    val sum_one = 400
    val sum_two = 74
    val divisor = 3
    val comparator = 158

    val p4 = new IfExpression(
      new ComparisonExpression(
        Type.EQ,
        new BinaryOperationExpression(
          Operator.DIV,
          new BinaryOperationExpression(
            Operator.PLUS,
            new IntConstant(sum_one),
            new IntConstant(sum_two)
          ),
          new IntConstant(divisor)
        ),
        new IntConstant(comparator)
      ),
      new IntConstant(sum_one + sum_two),
      new BinaryOperationExpression(
        Operator.DIV,
        new IntConstant(sum_one + sum_two),
        new IntConstant(0)
      )
    )

    val value = interpreter.evaluate(p4, environment, functions).asInstanceOf[IntValue]
    println("Result of P4 : " + value)
    val expected = sum_one + sum_two
    assert(expected == value.v)
  }

  test("Test Variable Read and Write Expression") {

    // Constants, BinaryOperation, Comparison, Conditional Execution, Variable
    // Input: let bot = 3 in
    //          (let bot = 2 in bot)
    //          +
    //          (if (bot == 0) then 474/0 else (400+74)/bot)
    // Output: IntValue{v=160} or IntValue{v=239}
    val val_one = 3
    val val_two = 2
    val sum = 474
    val sum_one = 400
    val sum_two = 74

    val p5 = new LetExpression(
      new Name("bot"),
      new IntConstant(val_one),
      new BinaryOperationExpression(
        Operator.PLUS,
        new LetExpression(
          new Name("bot"),
          new IntConstant(val_two),
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
            new IntConstant(sum),
            new IntConstant(0)
          ),
          new BinaryOperationExpression(
            Operator.DIV,
            new BinaryOperationExpression(
              Operator.PLUS,
              new IntConstant(sum_one),
              new IntConstant(sum_two)
            ),
            new VariableExpression(new Name("bot"))
          )
        )
      )
    )

    val value = interpreter.evaluate(p5, environment, functions).asInstanceOf[IntValue]
    println("Result of P5 : " + value)
    if (environment.isInstanceOf[LexicalScopedEnvironment]) {
      val expected = sum / val_one + val_two
      assert(expected == value.v)
    } else {
      val expected = sum / val_two + val_two
      assert(expected == value.v)
    }
  }

  test("Test Function Call Expression") {

    // Constants, BinaryOperation, Comparison, Conditional Execution, Variable, Functions
    // Input: function f(top,bot) :
    //          if (bot == 0) then 0 else top/bot
    //
    //        let bot = 3 in
    //          (let bot = 2 in bot)
    //          +
    //          (f(400+74,bot) + f(400+74,0))
    // Output: IntValue{v=160} or IntValue{v=239}
    val val_one = 3
    val val_two = 2
    val sum_one = 400
    val sum_two = 74

    val p6 = new LetExpression(
      new Name("bot"),
      new IntConstant(val_one),
      new BinaryOperationExpression(
        Operator.PLUS,
        new LetExpression(
          new Name("bot"),
          new IntConstant(val_two),
          new VariableExpression(new Name("bot"))
        ),
        new BinaryOperationExpression(
          Operator.PLUS,
          new FunctionCallExpression(
            new Name(functionName),
            new BinaryOperationExpression(
              Operator.PLUS,
              new IntConstant(sum_one),
              new IntConstant(sum_two)
            ),
            new VariableExpression(new Name("bot"))
          ),
          new FunctionCallExpression(
            new Name(functionName),
            new BinaryOperationExpression(
              Operator.PLUS,
              new IntConstant(sum_one),
              new IntConstant(sum_two)
            ),
            new IntConstant(0)
          )
        )
      )
    )

    val value = interpreter.evaluate(p6, environment, functions).asInstanceOf[IntValue]
    println("Result of P6 : " + value)
    if (environment.isInstanceOf[LexicalScopedEnvironment]) {
      val expected = (sum_one + sum_two) / val_one + val_two
      assert(expected == value.v)
    } else {
      val expected = (sum_one + sum_two) / val_two + val_two
      assert(expected == value.v)
    }
  }
}