package cw.week4

trait Expr {
  def eval: Int = this match {
    case Num(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  def show(): String = this match {
    case Num(n) => n.toString
    case Sum(e1, e2) => e1.show() + "+" + e2.show()
    case Prod(e1, e2) => e1.show() + "*" + e2.show()
    case Var(x) => x
  }
}

case class Num(n: Int) extends Expr {

}

case class Sum(e1: Expr, e2: Expr) extends Expr {
}

case class Prod(e1: Expr, e2: Expr) extends Expr {

}

case class Var(x: String) extends Expr {

}

object PatternMatching extends App {
  val foo = Num(4)
  println(foo.show())
  println((Prod(Num(4),Num(5))).show())
  println(Var("z").show())
}
