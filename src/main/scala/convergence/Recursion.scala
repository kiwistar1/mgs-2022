package convergence

object Recursion {
  // sum of list of Int
  def sumListOfInt(items: List[Int]): Int = {
    ???
  }

  // product of a list of Double
  def productListOfDouble(items: List[Int]): Int = {
    ???
  }

  // sum a list of anything that is summable
  def sumList[T](items: List[T], ident: T, add: (T, T) => T): T = {
    ???
  }

  def harmonicSum(items: List[Double]): Double = {
    ???
  }

  def removeFromList[T](items: List[T], deleteMe: T): List[T] = {
    ???
  }

  def removeOnce[T](items: List[T], deleteMe: T): List[T] = {
    ???
  }

  val students = List(
    "Drew",
    "Thomas",
    "Jasmine",
    "Gertrude",
    "Akhila",
    "Jaeho",
    "Gertrude",
    "Cole",
    "Gracyn",
    "Gertrude",
    "Rachele",
    "Sydney"
  )

  def main(argv: Array[String]): Unit = {
    println(removeOnce(students, "Gertrude"))

  }
}
