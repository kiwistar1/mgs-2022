package algebra

object Examples {
  
  import algebra.AbstractAlgebraFinite._

  def cartesianProduct[T](
      s: Set[T],
      mult: (T, T) => T
  ): (Set[(T, T)], ((T, T), (T, T)) => (T, T)) = {
    val newSet = for {
      x <- s
      y <- s
    } yield (x, y)
    def newMult(a: (T, T), b: (T, T)): (T, T) = {
      val (a1, a2) = a
      val (b1, b2) = b
      (mult(a1, b1), mult(a2, b2))
    }
    (newSet, newMult)
  }

  val rotationGroup = {
    def mult(a: String, b: String): String = {
      (a, b) match {
        case ("R0", b) => b
        case (_, "R0") => a

        case ("MY", "MYR90") => "R90"
        case ("MY", "MXR90") => "R270"
        case ("MY", "MX")    => "R180"
        case ("MY", "MY")    => "R0"
        case ("MY", "R270")  => "MXR90"
        case ("MY", "R180")  => "MX"
        case ("MY", "R90")   => "MYR90"

        case ("R90", "R90")   => "R180"
        case ("R90", "R180")  => "R270"
        case ("R90", "R270")  => "R0"
        case ("R90", "MX")    => "MYR90"
        case ("R90", "MY")    => "MXR90"
        case ("R90", "MXR90") => "MX"
        case ("R90", "MYR90") => "MY"

        case ("R180", "R90")   => "R270"
        case ("R180", "R180")  => "R0"
        case ("R180", "R270")  => "R90"
        case ("R180", "MX")    => "MY"
        case ("R180", "MY")    => "MX"
        case ("R180", "MXR90") => "MYR90"
        case ("R180", "MYR90") => "MXR90"

        case ("R270", "R90")   => "R0"
        case ("R270", "R180")  => "R90"
        case ("R270", "R270")  => "R180"
        case ("R270", "MX")    => "MXR90"
        case ("R270", "MY")    => "MYR90"
        case ("R270", "MXR90") => "MY"
        case ("R270", "MYR90") => "MX"

        case ("MX", "R90")   => "MXR90"
        case ("MX", "R180")  => "MY"
        case ("MX", "R270")  => "MYR90"
        case ("MX", "MX")    => "R0"
        case ("MX", "MY")    => "R180"
        case ("MX", "MXR90") => "R90"
        case ("MX", "MYR90") => "R270"

        case ("MXR90", "R90")   => "MY"
        case ("MXR90", "R180")  => "MYR90"
        case ("MXR90", "R270")  => "MX"
        case ("MXR90", "MX")    => "R270"
        case ("MXR90", "MY")    => "R90"
        case ("MXR90", "MXR90") => "R0"
        case ("MXR90", "MYR90") => "R180"

        case ("MYR90", "R90")   => "MX"
        case ("MYR90", "R180")  => "MXR90"
        case ("MYR90", "R270")  => "MY"
        case ("MYR90", "MX")    => "R90"
        case ("MYR90", "MY")    => "R270"
        case ("MYR90", "MXR90") => "R180"
        case ("MYR90", "MYR90") => "R0"

        case _ => ???
      }
    }

    val rotations =
      Set("R0", "R90", "R180", "R270", "MX", "MY", "MXR90", "MYR90")
    (rotations, mult _)
  }

  val kleinGroup = (
    Set(1, 2, 3, 4), {
      def add(a: Int, b: Int): Int = {
        if (a == 1)
          b
        else if (b == 1)
          a
        else if (a == b)
          1
        else
          (Set(2, 3, 4) - a - b).head
      }

      add _
    }
  )

  def zMod(k: Int): (Set[Int], (Int, Int) => Int, (Int, Int) => Int) = {
    (
      (0 until k).toSet,
      (a: Int, b: Int) => (a + b) % k,
      (a: Int, b: Int) => (a * b) % k
    )
  }
}
