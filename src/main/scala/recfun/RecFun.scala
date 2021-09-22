package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c>r) 0
    else if (c==0 || c==r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check_balance(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) count == 0
      else
        if (chars.head == '(') check_balance(chars.tail, count - 1)        
        else
          if (chars.head == ')') count < 0 && check_balance(chars.tail, count + 1)
          else check_balance(chars.tail, count)
    }      
    check_balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && !coins.isEmpty)
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    else 0
  }
