package cs.edu.uic.interpreter

class Name(val theName: String) {
  override def toString: String = "Name{" + theName + '}'

  override def equals(obj: Any): Boolean = {
    if (obj == null || this.getClass != obj.getClass) return false
    val name = obj.asInstanceOf[Name]
    theName.equals(name.theName)
  }
}
