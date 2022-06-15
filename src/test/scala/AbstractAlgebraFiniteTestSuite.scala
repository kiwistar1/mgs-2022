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
import algebra.Examples._
import algebra.AbstractAlgebraFinite._

class AbstractAlgebraFiniteTestSuite extends munit.FunSuite {
  test("monoid") {
    assert(isMonoid(kleinGroup._1, kleinGroup._2))
    assert(isMonoid(rotationGroup._1, rotationGroup._2))
    for {
      k <- 1 to 7
      triple = zMod(k)
    } {
      assert(isMonoid(triple._1, triple._2))
      assert(isMonoid(triple._1, triple._3))
    }
    for { n <- 3 to 10 } {
      val s = (0 until n).toSet

      def subtract(a: Int, b: Int): Int = {
        (a - b + n) % n
      }

      assert(!isMonoid(s, subtract), s"should not be a monoid for n=$n s=$s")
    }
    val kleinX2 = cartesianProduct(kleinGroup._1, kleinGroup._2)
    assert(isMonoid(kleinX2._1, kleinX2._2))
    val rotationX2 = cartesianProduct(kleinX2._1, kleinX2._2)
    assert(isMonoid(rotationX2._1, rotationX2._2))
  }

  test("group") {
    assert(Some(1) == findIdentity(kleinGroup._1, kleinGroup._2))
    assert(isGroup(kleinGroup._1, kleinGroup._2))
    assert(Some("R0") == findIdentity(rotationGroup._1, rotationGroup._2))
    assert(isGroup(rotationGroup._1, rotationGroup._2))
  }

  test("abelian") {
    assert(isAbelian(kleinGroup._1, kleinGroup._2))
    assert(!isAbelian(rotationGroup._1, rotationGroup._2))
  }

  test("ring") {
    for {
      k <- 2 to 7
      triple = zMod(k)
    } {
      assert(
        isRing(triple._1, triple._2, triple._3),
        s"should be a ring if k=$k, triple=${triple._1}"
      )
    }
  }

  test("field") {
    for {
      k <- 2 to 9
      (s, add, times) = zMod(k)
    } {
      if (Seq(2, 3, 5, 7).contains(k)) { // primes
        assert(
          isField(s, add, times),
          s"should be a field if k is prime, k=$k, s=$s"
        )
      } else // composite not a field
        assert(
          !isField(s, add, times),
          s"should not be a field if k is composite, k=$k"
        )
    }
    assert(!isField(rotationGroup._1, rotationGroup._2, rotationGroup._2))
  }
}
