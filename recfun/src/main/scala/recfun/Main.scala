package recfun
import common._
import scala.annotation._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(countChange(4,List(1, 2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def iBalance(cc: Int, body: List[Char]): Boolean = {
      if (body.isEmpty && cc == 0) return true
      if (cc < 0 || body.isEmpty) return false
      if (body.head == '(')
        iBalance(cc + 1, body.tail)
      else if (body.head == ')')
        iBalance(cc - 1, body.tail)
      else
        iBalance(cc, body.tail)
    }
    iBalance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 && coins.isEmpty) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins     ) +
         countChange(money             , coins.tail)
  }
}
