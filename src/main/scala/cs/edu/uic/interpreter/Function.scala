package cs.edu.uic.interpreter

import scala.annotation.tailrec

class Function(val name: Name, val body: Expression, val formalNamesOfArguments: Name*) {
}

object Function {
  @tailrec
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

