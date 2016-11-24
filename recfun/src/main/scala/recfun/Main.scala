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
    def pascal(c: Int, r: Int): Int = {
      if (r == 0 || c == 0 || c == r) 1 else {pascal(c-1, r-1) + pascal(c, r-1)}
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def stack_index(index:Int, chars: List[Char]): Int = {
        if (index < 0 || chars.isEmpty) index
        else if (chars.head == '(') stack_index(index+1, chars.tail)
        else if(chars.head == ')') stack_index(index-1, chars.tail)
        else stack_index(index, chars.tail)
        }

        stack_index(0, chars) == 0
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money == coins.head) 1 + countChange(money, coins.tail)
      else if (money > coins.head) countChange(money-coins.head, coins) + countChange(money, coins.tail)
      else countChange(money, coins.tail)

    }
  }
