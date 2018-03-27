package cw.week5

class ExperimentList[T] {

  def reduceLeftX (vals: List[T]) (op: (T,T) => T): T = vals match {
    case Nil => throw new Error("reduceLeft on empty list.")
    case List(x,y) => op(x,y)
    case x :: xs => foldLeftX(xs, op(x,x)) (op)
  }

  def foldLeftX[U,T] (vals: List[T], z: U) (op: (U, T) => U): U = vals match {
    case Nil => z
    case List(x) => op(z,x)
    case x :: xs => foldLeftX(xs, op(z, x)) (op)
  }

  def reduceRightX (vals: List[T]) (op: (T,T) => T): T = vals match {
    case Nil => throw new Error("Nil.reduceRight")
    case List(x) => x
    case x :: xs => op(x, reduceRightX(xs)(op))
  }


  def foldRightX[U] (vals: List[T], z: U) (op: (U,T) => U): U = vals match {
    case Nil => z
    case List(x) => op(z,x)
    case x :: xs => op(foldRightX(xs,z)(op), x)
  }
}

object foo extends App {
  val foo = new ExperimentList[Int]

  val list1 = List(2,1,3,5,4,2,4,4,0)

  println(foo.reduceLeftX(list1)((x,y) => x+y))
  println(foo.reduceRightX(list1)((x,y) => x+y))

  val intList = List[Int]()
  println(foo.foldRightX(list1, intList) ((list,x) => x :: list ) )
}