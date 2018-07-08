object temp extends App{

  abstract class Stack[A] {
    def push(x: A): Stack[A] = new NonEmptyStack[A](x, this)
    def isEmpty: Boolean
    def top: A
    def pop: Stack[A]
  }
  class EmptyStack[A] extends Stack[A] {
    def isEmpty = true
    def top = sys.error("EmptyStack.top")
    def pop = sys.error("EmptyStack.pop")
  }
  class NonEmptyStack[A](elem: A, rest: Stack[A]) extends Stack[A] {
    def isEmpty = false
    def top = elem
    def pop = rest
  }


  def isPrefix[A](p: Stack[A], s: Stack[A]): Boolean = {
    p.isEmpty ||
      p.top == s.top && isPrefix[A](p.pop, s.pop)
  }

  def iter[A](k: Stack[A]): Boolean = {
    if(k.isEmpty) k.isEmpty
    else {
      println(k.top)
      iter[A](k.pop)
    }
  }

  val temp = new EmptyStack[Int]

  val x = temp.push(4).push(10).push(14)

  iter(x)

  val s1 = new EmptyStack[String].push("abc")
  val s2 = new EmptyStack[String].push("abx").push(s1.top)
  println(isPrefix(s1, s2))

}
