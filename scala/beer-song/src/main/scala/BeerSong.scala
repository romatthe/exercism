object BeerSong {

  def recite(start: Int, passes: Int): String = {
    (start until start - passes by -1).map(rhyme).reduce((xs, s) => xs + s).dropRight(1)
  }

  def rhyme(line: Int): String = {
    val first = line match {
      case 0 => "No more bottles of beer on the wall, no more bottles of beer."
      case 1 => "1 bottle of beer on the wall, 1 bottle of beer."
      case n => f"$n bottles of beer on the wall, $n bottles of beer."
    }

    val second = line match {
      case 0 => "Go to the store and buy some more, 99 bottles of beer on the wall."
      case 1 => "Take it down and pass it around, no more bottles of beer on the wall."
      case 2 => f"Take one down and pass it around, 1 bottle of beer on the wall."
      case n => f"Take one down and pass it around, ${n - 1} bottles of beer on the wall."
    }

    first + "\n" + second + "\n\n"
  }

}
