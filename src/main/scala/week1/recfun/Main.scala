package week1.recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == 1) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def checkParenth(chars: List[Char], openLeftParenthCount: Int, openRightParentCount: Int): Boolean = {

      if (openRightParentCount > openLeftParenthCount) false
      else if (chars.isEmpty)  openLeftParenthCount == openRightParentCount
      else if (chars.head == '(') checkParenth(chars.tail, openLeftParenthCount + 1, openRightParentCount)
      else if (chars.head == ')') checkParenth(chars.tail, openLeftParenthCount, openRightParentCount + 1)
      else checkParenth(chars.tail, openLeftParenthCount, openRightParentCount)
    }

    checkParenth(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var totalAmount = 0
    def check(money: Int, coins: List[Int]) {
      if (coins.nonEmpty)
        if (money>coins.head) {
          check(money-coins.head, coins)
          check(money,coins.tail)
        }
        else if (money<coins.head) {
          check(money,coins.tail)
        }
        else if (money-coins.head == 0) {
          totalAmount += 1
        }
    }
    check(money,coins.sorted)
    totalAmount
  }
}
