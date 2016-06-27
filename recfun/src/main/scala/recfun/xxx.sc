    def countChange(money: Int, coins: List[Int]): Int = {
      def inner(money: Int, coins: List[Int]): Int =
        if (money < 0)
          0
        else if (money == 0)
          1
        else if (coins.isEmpty)
          0
        else
          coins.tails.map(s =>
            if (s.isEmpty)
              0
            else
              /*countChange(money - s.head, s.tail) +*/
              countChange(money - s.head, s)).sum

      inner(money, coins.sorted)
    }

countChange(4, List(1,2))
countChange(5, List(2,3))
