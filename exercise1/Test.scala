import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._

object Test {
  def main(args : Array[String]) {
    val now = new Date
    val df = getDateInstance(LONG, Locale.FRANCE)
    println(df format now)
    val x = 3
    if (x >= 3)
      println("haha")
    println(balance("".toList))
  }
  def sqrt(x:Double):Double = {
    def abs(x:Double):Double =
      if (x >=0) x
      else -x
    def isGoodEnough(guess:Double,x:Double):Boolean =
      abs(guess * guess - x) < 0.0001
    def improve(guess:Double,x:Double):Double =
      (guess + x / guess) / 2

    def sqrtIter(guess:Double, x:Double):Double =
      if (isGoodEnough(guess,x)) guess
      else sqrtIter(improve(guess,x),x)

    sqrtIter(1.0,x)
  }
  def balance(chars: List[Char]): Boolean = {
    def balanNow(chars: List[Char],cnt: Int): Boolean = {
      if (chars.isEmpty) {
        if (cnt == 0) true
        else false
      }
      else {
        if (chars.head == '(')
          balanNow(chars.tail, cnt + 1)
        else if (chars.head == ')') {
          if (cnt > 0) balanNow(chars.tail, cnt - 1)
          else false
        }
        else balanNow(chars.tail, cnt)
      }
    }
    balanNow(chars, 0)
  }

  def pascal(c: Int, r: Int):Int =
    if (c == r) 1
    else if (c == 0) 1
    else pascal(c-1,r-1) + pascal(c,r-1)

  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money <= 0 && !coins.isEmpty) 0
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)

}
