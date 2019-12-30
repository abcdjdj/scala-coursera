package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if(c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_helper(chars: List[Char], counter: Int): Int =
        if(chars.isEmpty || counter < 0)
            counter
        else if(chars.head == '(')
            balance_helper(chars.tail, counter + 1)
        else if(chars.head == ')')
            balance_helper(chars.tail, counter - 1)
        else
            balance_helper(chars.tail, counter)
    
    balance_helper(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) {
        1
    } else if(coins.isEmpty) {
        0
    } else {
        def exclude = countChange(money, coins.tail)
        def include =
            if(money - coins.head >= 0) countChange(money - coins.head, coins) else 0
        exclude + include
    }
        
  }
}
