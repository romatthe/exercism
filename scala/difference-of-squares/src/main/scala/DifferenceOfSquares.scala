object DifferenceOfSquares {

  def squareOfSum(n: Int): Int = {
    val sum = (1 to n).sum
    sum * sum
  }

  def sumOfSquares(n: Int): Int = (1 to n).map(i => i * i).sum

  def differenceOfSquares(n: Int): Int = Math.abs(sumOfSquares(n) - squareOfSum(n))
}
