package hw.week2.funsets

object Main extends App {
  import FunSets._
  //println(contains(singletonSet(1), 1))
  //println(contains(union(singletonSet(1), singletonSet(2)), 2)) // -> true
  //println(contains(union(singletonSet(1), singletonSet(2)), 3)) // -> false

  def set1 = (x: Int) => x < 10 && x > -10
  def set2 = (x: Int) => x > 5 && x < 15

  assert(contains(set1, 4))
  assert(!contains(set2, 16))

  // get a characterstic function for the even sets
  def evenSet = filter(set1, x => x%2 == 0)

  assert(contains(evenSet, 6))   // even, yes!
  assert(!contains(evenSet, 5))  // odd, no!
  assert(!contains(evenSet, 20)) // set1 does NOT originally contain 20
  // nice !

  assert(forall(set2, x => x < 20 && x > 5))   // all elements in set2 must be greater than 5 and less than 20
  assert(exists(evenSet, x => x%8 == 0))       // there should be at least one element in evenSet divisible by 8 (i.e. 8 itself)
  println(printSet(evenSet))

  def squaredSet = map(set2, x => x * x)
  println(printSet(squaredSet))
  assert(contains(squaredSet, 100))
  assert(!contains(map(set2, x => x * x * x), 100))  // elements in set2 are taken to third power. 100 should no longer be there..
  // cool ..

}
