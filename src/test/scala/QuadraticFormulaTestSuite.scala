// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


import functions.QuadraticFormula._
import org.scalatest.funsuite.AnyFunSuite

import scala.math.abs

class QuadraticFormulaTestSuite extends AnyFunSuite {

  test("quadraticFormula") {
    assert(quadraticFormula(1, 0, 0) == List(0.0))
    assert(
      quadraticFormula(1, 2, 3).size == 0
    )
    assert(quadraticFormula(-1,2,3).size==2)
    assert(quadraticFormula(1,-2,-3).size==2)
    assert(quadraticFormula(1,0,0).size == 1)
  }

  test("quadraticFormula check roots") {
    // in class we develop a property based test
    // to check the quadraticFormula function
    val epsilon = 0.001

    def check(r1: Double, r2: Double,
              a: Int, b: Int, c: Int): Unit = {
      def p1(x: Double): Double = {
        a * (x - r1) * (x - r2)
      }

      def p2(x: Double): Double = {
        a * x * x + b * x + c
      }

      for {i <- -100 to 100
           x = i.toDouble / 13} {
        assert(abs(p1(x) - p2(x)) < epsilon,
               s"\n  wrong roots r1=$r1, r2=$r2\n  for a=$a b=$b c=$c,\n  with x=$x  p1(x)=${p1(x)} p2(x)=${p2(x)}")
      }
    }

    for {a <- (-10 to 10)
         if a != 0
         b <- (-10 to 10)
         c <- (-10 to 10)
         roots = quadraticFormula(a, b, c)
         } {
      roots match {
        case List() => ()
        case List(r1, r2) => check(r1, r2, a, b, c)
        case List(r) => check(r, r, a, b, c)
        case _ => ???
      }
    }
  }
}
