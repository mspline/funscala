    def countChange(money: Int, coins: List[Int]): Int = {
      def countSortedChange(money: Int, sortedCoins: List[Int]): Int =
        if (money == 0)
          1
        else if (money < 0 || sortedCoins.isEmpty)
          0
        else
          sortedCoins.tails.toList.init.map(
            s => countChange(money - s.head, s)).sum

      countSortedChange(money, coins.sorted)
    }

countChange(4, List(1,2))
countChange(5, List(2,3))
