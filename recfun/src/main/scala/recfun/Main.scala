package recfun

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
      if (c == 0 || c == r)
        1
      else
        pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceImpl(chars: List[Char], count: Int): Boolean =
        if (count < 0)
          false
        else
          if (chars.isEmpty)
            count == 0
          else
            balanceImpl(
              chars.tail,
              count + (if (chars.head == '(') 1 else if (chars.head == ')') -1 else 0))

      balanceImpl(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countSortedChange(money: Int, sortedCoins: List[Int]): Int =
        if (money == 0)
          1
        else if (money < 0 || sortedCoins.isEmpty)
          0
        else
          sortedCoins.tails.toList.init.map(
            s => countSortedChange(money - s.head, s)).sum

      countSortedChange(money, coins.sorted)
    }
  }
