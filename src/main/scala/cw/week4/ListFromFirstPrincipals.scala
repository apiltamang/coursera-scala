package cw.week4

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def foreach(f: T => Unit): Unit = {
    if(!isEmpty){
      f(head)
      tail.foreach(f)
    }
  }
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  /**
    * The interface we expect is: 
    * 
    * List()
    * List(1)
    * List(2,3) etc.
    * 
    * Now, the truth is that the 'apply' method is called as follows
    * when the following invocations are made:
    * 
    * List() = List.apply()
    * List(1) = List.apply(1)
    * List(2) = List.apply(1,2)
    * 
    * So, we can simply override the apply methods to achieve the
    * desired interfaces.
    */

  def apply[T]: List[T] = new Nil[T]
  def apply[T](x: T): List[T] =  new Cons[T](x, new Nil[T])
  def apply[T](x: T, y: T): List[T] = new Cons[T](x, new Cons[T](y, new Nil[T]))
}

object ListFromFirstPrincipals extends App {
  val x0 = List()
  val x1 = List(1)
  val x2 = List(55,66)

  x0.foreach(x => println(x))
  x1.foreach(x => println(x))
  x2.foreach(x => println(x))
}
