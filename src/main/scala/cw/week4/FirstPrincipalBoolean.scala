package cw.week4

abstract class Bool {

  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => Bool): Bool = ifThenElse(x, True)

  def || (x: => Bool): Bool = ifThenElse(True, x)

  def unary_! : Bool           = ifThenElse(False, True)

  def == (x: Bool): Bool    = ifThenElse(x, x.unary_!)

  def != (x: Bool): Bool    = ifThenElse(x.unary_!, x)

}

object True extends Bool {
  override def ifThenElse[T](t: => T, e: => T) = t
}

object False extends Bool {
  override def ifThenElse[T](t: => T, e: => T) = e
}

object FirstPrincipalBool extends App {
  val myTrue = True
  val myFals = False

  assert((myTrue && myTrue).ifThenElse("success!", "failure!") == "success!")

  assert((myTrue && myFals).ifThenElse("success!", "failure!") == "failure!")

  (!myFals).ifThenElse(println("success!"), println("failure!"))

}