package algebra

object Complex {

  import Matrix._

  type COMPLEX = (Double, Double)

  lazy val i: COMPLEX = ???

  // unit for complex multiplication
  lazy val one: COMPLEX = ???

  // unit for complex addition
  lazy val zero: COMPLEX = ???

  def complexToMatrix(z: COMPLEX): MATRIX = {
    ???
  }

  def matrixToComplex(m: MATRIX): COMPLEX = {
    assert(dimension(m) == 2)
    ???
  }

  def complexAdd(a: COMPLEX, b: COMPLEX): COMPLEX = {
    ???
  }

  def complexScale(alpha: Double, m: COMPLEX): COMPLEX = {
    ???
  }

  def complexSubtract(a: COMPLEX, b: COMPLEX): COMPLEX = {
    ???
  }

  def complexMultiply(a: COMPLEX, b: COMPLEX): COMPLEX = {
    ???
  }

  def complexPower(m: COMPLEX, p: Int): COMPLEX = {
    ???
  }

  def complexAlmostEqual(a: COMPLEX, b: COMPLEX, epsilon: Double): Boolean = {
    ???
  }

  def main(argv: Array[String]): Unit = {
    // verify that i*i = -1
    ???
  }
}
