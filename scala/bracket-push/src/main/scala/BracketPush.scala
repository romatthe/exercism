object BracketPush {
  val matches = Map(
    '[' -> ']',
    '{' -> '}',
    '(' -> ')',
    ']' -> '[',
    '}' -> '{',
    ')' -> '('
  )

  def isPaired(str: String) = {
    Stream
      .range(0, str.length)
      .map(i => isSame(str, i))
      .forall(_ == true)
  }

  def isSame(str: String, index: Int): Boolean = {
    val bracket = str.charAt(index)
    val opposite = matches(bracket)
    opposite == str.charAt(str.length - 1 - index)
  }
}
