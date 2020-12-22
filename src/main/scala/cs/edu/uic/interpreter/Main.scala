package cs.edu.uic.interpreter

object Main {

  def main(args: Array[String]): Unit = {
    println("Just a Interpreter")
    println("Check out the Unit Tests to learn how it works")
  }

  def getInterpreter: Interpreter = {
    new Interpreter()
  }
}
