package scash.warhorse.core.script

object StackInterpreter {

  def opDup(stack: Stack) = stack.peek.map(stack.push)
}
