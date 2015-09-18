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
   * Exercise 1x
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def isCheck(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head == '(' && open >= 0) isCheck(chars.tail, open + 1)
      else if (chars.head == ')') isCheck(chars.tail, open - 1)
      else
        isCheck(chars.tail, open);
    }

    isCheck(chars, 0);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def recur(money: Int, length: Int): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else if (length < 0 && money > 0) 0
      else

        recur(money, length - 1) + recur(money - coins(length), length)
    }

    recur(money, coins.length-1);
  }

}
