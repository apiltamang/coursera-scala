package cw.week5

object PackEx extends App {

  val data = List("a","a","a","b","c","c","a")

  def pack[T](xs: List[T]) : List[List[T]] = xs match {
    case Nil => Nil
    case x :: _ =>
      val (first, rest) = xs span(y => y==x)
      first :: pack(rest)
  }

  println(pack(data)) // List(List(a, a, a), List(b), List(c, c), List(a))

  def encode[T](xs: List[T]): List[(T,Int)] =
    pack(xs).map(ys => (ys.head, ys.length))

  println(encode(data))
}
