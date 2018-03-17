package hw.week1.recfun

object Recursions extends App {


    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    assert(balance("ab(cd)".toList))
    assert(!balance("ab(".toList))
    assert(balance("ab(c(de))".toList))
    assert(!balance("abc(())ef)".toList))
    assert(!balance("abc())(".toList))

    val items = List(1,2,3,4,5,6)
    println(getBigOrEqualList(4,items))
    println(getBigOrEqualList(5,items))

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 && c == 0) return 1
    if (c > r || c == -1) return 0

    return pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def helper(charList: List[Char], clicker: Int): Boolean = {

      if(clicker < 0)
        return false

      else if(charList.isEmpty && clicker > 0)
        return false

      else if(charList.isEmpty && clicker == 0)
        return true

      else if(openParen(charList.head))
        return helper(charList.tail, clicker+1)

      else if(closeParen(charList.head))
        return helper(charList.tail, clicker-1)

      else
        return helper(charList.tail, clicker)
    }

    return helper(chars, 0)

  }

  def openParen(c: Char): Boolean = {
    if (c == '(')
      return true
    else
      return false
  }

  def closeParen(c: Char): Boolean = {
    if (c == ')')
      return true
    else
      return false
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countHelper(money: Int, coins: List[Int], counter: Int): Int = {
      if (money == 0)
        return counter+1
      else if(coins.isEmpty || money < 0)
        return counter
      else
        return countHelper(money - coins.head, coins, counter) + countHelper(money, coins.tail, counter)
    }

    countHelper(money, coins, 0)
  }

  def getBigOrEqualList(x: Int, items: List[Int]): List[Int] = if(x > items.head) getBigOrEqualList(x, items.tail) else items
}
