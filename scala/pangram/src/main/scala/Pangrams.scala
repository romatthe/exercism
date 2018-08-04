object Pangrams {
  val alpha = "abcdefghijklmnopqrstuvwxyz" split ""

  def isPangram(input: String): Boolean = alpha forall (input.toLowerCase contains)
}

