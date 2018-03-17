package hw.week2.funsets

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = (Int => Boolean)

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */

  def singletonSet(elem: Int): Set = ((x: Int) => x == elem)
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether ALL bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if (a > bound)
          return true
        else if (s(a))
          return (p(a) && iter(a+1))
        else
          iter(a+1)
      }
    return iter(-bound)
  }
  
  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    * Approach: forall(s, p) checks and sees if ALL numbers in set 'S' satisfies predicate 'p'.
    * To find if at least one element in 's' satisfies 'p', we want to find if ALL of the
    * elements in 'S' satisfies the INVERSE of 'p', then just takes it negation :)
   */

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
  
  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    * ------------------------- SOLUTION APPROACH ---------------------
    * Take the example of f such that f(x) = x * x, and assumes that
    * {2,3,7, 8} are members of s.
    *
    * The usage of the below would be:
    * squared = map(s, x => x * x), then
    *
    * println(squared(3))  -> false
    * println(squared(4))  -> true, because 2 exists in s
    * println(squared(16)) -> false, because 4 does NOT exist in s
    * println(squared(49)) -> true, because 7 exists in s
    * ----------------------------------------------------------------
   */
  def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, x => f(x) == y)

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
