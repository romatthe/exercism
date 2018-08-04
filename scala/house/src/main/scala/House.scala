object House {
  val now = "This is %s\n"
  val before = "%s %s"

  val text = List(
    ("that Jack built.", "the house"),
    ("that lay in", "the malt"),
    ("that ate", "the rat"),
    ("that killed", "the cat"),
    ("that worried", "the dog"),
    ("that tossed", "the cow with the crumpled horn"),
    ("that milked", "the maiden all forlorn"),
    ("that kissed", "the man all tattered and torn"),
    ("that married", "the priest all shaven and shorn"),
    ("that woke", "the rooster that crowed in the morn"),
    ("that kept", "the farmer sowing his corn"),
    ("that belonged to", "the horse and the hound and the horn")
  )

  private def verse(n: Int): String = {
    val body = n
        .to(0, -1)
        .map(line => before.format(text(line)._2, text(line)._1))
        .mkString(" ")

    now.format(body)
  }

  def recite(from: Int, to: Int): String =
    (from - 1)
      .until(to)
      .map(verse)
      .mkString + "\n"
}
