package us.nuernberg.hackerrank.birthdaycandles

/**
 *  You are in charge of the cake for your niece's birthday and have decided the cake will have one candle for each
 *  year of her total age.
 *  When she blows out the candles, she’ll only be able to blow out the tallest ones.
 *  Your task is to find out how many candles she can successfully blow out.
 *
 *  For example, if your niece is turning 4 years old, and the cake will have 4 candles of height 4, 4, 1, 3,
 *  she will be able to blow out 2 candles successfully,
 *  since the tallest candles are of height 4 and there are 2 such candles.
 *
 *  = Function Description =
 *
 *  Complete the function birthdayCakeCandles in the editor below.
 *  It must return an integer representing the number of candles she can blow out.
 *
 *  birthdayCakeCandles has the following parameter(s):
 *
 *  - ar: an array of integers representing candle heights
 *
 *  = Input Format =
 *
 *  The first line contains a single integer, n, denoting the number of candles on the cake.
 *  The second line contains n space-separated integers, where each integer i describes the height of candle i.
 *
 *  = Constraints =
 *
 *  - 1 <= n <= 10^5^
 *  - 1 <= ar[i] <= 10^7^
 *
 *  = Output Format =
 *
 *  Print the number of candles that can be blown out on a new line.
 *
 *  = Sample Input 0 =
 *
 *  4
 *  3 2 1 3
 *
 *  = Sample Output 0 =
 *
 *  2
 *
 *  = Explanation 0 =
 *
 *  We have one candle of height 1, one candle of height 2, and two candles of height 3.
 *  Your niece only blows out the tallest candles, meaning the candles where height = 3.
 *  Because there are 2 such candles, we print 2 on a new line.
 */
object Solution {

  def main(args: Array[String]): Unit = {
    val _ :: line :: Nil = io.Source.stdin.getLines.toList
    val heights = line.split(" ").map(_.toInt).toList
    println(solve(heights))

  }

  def solve(rest: List[Int], currMax: Int = 0, currNum: Int = 0): Int =
    rest match {
      case Nil => currNum
      case h :: t =>
        if (h > currMax) {
          solve(t, h, 1)
        } else if (h == currMax) {
          solve(t, currMax, currNum + 1)
        } else {
          solve(t, currMax, currNum)
        }
    }

}

object Test {

  def main(args: Array[String]): Unit = {
    println(Solution.solve(List(3, 2, 1, 3)))
  }
}
