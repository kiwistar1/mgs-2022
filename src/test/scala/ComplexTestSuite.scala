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

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class ComplexTestSuite extends munit.FunSuite {

  import algebra.Complex._

  test("complexAdd") {
    assert((4, 7) == complexAdd((1, 2), (3, 5)))
  }

  test("complexSubtract") {
    assert((-2, -3) == complexSubtract((1, 2), (3, 5)))
  }

  test("complexMultiply") {
    assert((-5, 10) == complexMultiply((1, 2), (3, 4)))
  }

  test("i*i = -1") {
    assert((-1, 0) == complexMultiply((0, 1), (0, 1)))

    // ??? // how can you test that i*i == -1
  }

  test("complex multiplication commutative") {
    val rand = new Random()
    for {_ <- 0 to 1000
         a = (rand.between(-10, 10).toDouble, rand.between(-10, 10).toDouble)
         b = (rand.between(-10, 10).toDouble, rand.between(-10, 10).toDouble)
         } assert(complexMultiply(a, b) == complexMultiply(b, a))
  }
}
