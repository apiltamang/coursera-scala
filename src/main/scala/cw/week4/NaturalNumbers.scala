package cw.week4

abstract class Nat {
  def isZero: scala.Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero = true

  override def predecessor: Nat = throw new NoSuchElementException("calling: 0.predecessor.")

  override def successor: Nat = new Next(this)

  override def + (that: Nat): Nat = that

  override def - (that: Nat): Nat = throw new NoSuchElementException("calling: 0 - nat.")

  override def toString: String = "0"
}

class Next(n: Nat) extends Nat {
  override def isZero: scala.Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = new Next(this)

  override def + (that: Nat): Nat = new Next(n + that)

  override def - (that: Nat): Nat = if (that.isZero) this else (n - that.predecessor)

  override def toString: String = n.toString + "+1"
}

object NaturalNumbers extends App {
  val zero = Zero
  val one = new Next(zero)
  val two = new Next(one)
  val three = new Next(two)
  val five = new Next(new Next(three))

  println(zero)
  println(one)
  println(two)
  println(five)

  assert((one + two).toString == three.toString)
  println((five - one).toString)
}