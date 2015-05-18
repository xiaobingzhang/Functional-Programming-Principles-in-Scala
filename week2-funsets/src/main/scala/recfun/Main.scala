package scala

//TODO run command ctr+shift+f10
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
      if(c==0||c==r) 1
      else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */
//  def balance(chars: List[Char]): Boolean ={
//    def balanced(chars: List[Char],open:Int):Boolean=
//      if(chars.isEmpty) open == 0
//      else if(chars.head == '(') balanced(chars.tail,open+1)
//      else if(chars.head == ')') open>0 &&balanced(chars.tail,open-1)
//      else balanced(chars.tail,open)
//    balanced(chars,0)
//  }
  def balance(chars: List[Char]): Boolean = {
     def balanced(chars: List[Char], open: Int): Boolean =
      chars match {
        case      Nil => open == 0
        case '(' :: t => balanced(t, open + 1)
        case ')' :: t => open > 0 && balanced(t, open - 1)
        case   _ :: t => balanced(t, open)
      }

    balanced(chars, 0)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    money match{
      case 0  => 1
      case x  if x < 0 => 0
      case x if x >=1 && coins.isEmpty => 0
      case _ => countChange(money,coins.tail)+countChange(money-coins.head,coins)
    }
  }


}
