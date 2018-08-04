import scala.util.Random

object Robot {
  val alpha: List[Char] = ('A' to 'Z') toList
  val digits: List[Char] = ('0' to '9') toList

  def getNewName: String = names.take(1).mkString

  private def names: Stream[String] = Stream.continually(generateRandomName).distinct

  private def generateRandomName: String = {
    Random.shuffle(alpha).take(2).mkString + Random.shuffle(digits).take(3).mkString
  }
}

class Robot() {
  var name: String = Robot.getNewName

  def reset(): Unit = {
    name = Robot.getNewName
  }
}
