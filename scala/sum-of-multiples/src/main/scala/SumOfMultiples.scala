object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = {
    val multiples = (base: Int, limit: Int) => Stream.from(1)
      .map(n => n * base)
      .takeWhile(n => n < limit)
      .toList

    factors
      .flatMap(n => multiples(n, limit))
      .sum
  }
}