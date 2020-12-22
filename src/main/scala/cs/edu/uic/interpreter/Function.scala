package cs.edu.uic.interpreter

class Function(val name: Name, val body: Expression, val formalNamesOfArguments: Name*) {
}

object Function {
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

