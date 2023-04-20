case class Position(x: Int, y: Int) {
  def X(): Int = x - 1
  def Y(): Int = y - 1
}

object Position {
  val alphabet_mapping: Map[Char, Int] = Map(
    'a' -> 1,
    'b' -> 2,
    'c' -> 3,
    'd' -> 4,
    'e' -> 5,
    'f' -> 6,
    'g' -> 7,
    'h' -> 8
  )
  def fromAlg(s: String): Position =
    new Position(
      alphabet_mapping.getOrElse(s(0), 0),
      s(1).toString.toInt
    )
}
