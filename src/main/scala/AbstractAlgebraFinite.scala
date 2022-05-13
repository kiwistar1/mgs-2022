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

object AbstractAlgebraFinite {
  // TASK: search the set s for an identity element,
  //    return None if no such element exists
  //    otherwise return that element wrapped in Some(...)
  def findIdentity[T](s: Set[T], op: (T, T) => T): Option[T] = {
    // The identity is guaranteed to be unique if it exists.
    // Because if there are two identifies e1 and e2, then
    // op(e1,e2)=e1 and op(e1,e2)=e2, by definition of identity.
    // so e1=e2.
    s.find(e => s.forall(a => op(a,e) == a && op(e,a) == a))
  }

  // TASK: Given a set and binary operation, detect exhaustively
  //   whether it is a monoid.  I.e., check all possible cases
  //   of the monoid axioms.
  def isMonoid[T](s: Set[T], op: (T, T) => T): Boolean = {
    // has identity ?
    findIdentity(s,op) != None &&
      // is closed ?
      s.forall(a => s.forall(b => s.contains(op(a,b)))) &&
      // is associative ?
      s.forall(a =>
                 s.forall(b =>
                            s.forall(c =>
                                       op(a,op(b,c)) == op(op(a,b),c))))
  }

  // TASK: Given a set and binary operation, detect exhaustively
  //   whether it is a group.  I.e., check all possible cases
  //   of the group axioms.   Make use of the isMonoid function.
  def isGroup[T](s: Set[T], op: (T, T) => T): Boolean = {
    isMonoid(s, op) && {
      val Some(e) = findIdentity(s, op)
      // is every element is invertible
      s.forall(a =>
                 s.exists(b =>
                            op(a, b) == e && op(b, a) == e))
    }
  }

  // TASK: Given a set and binary operation, detect exhaustively
  //   whether the operation is commutative
  def isAbelian[T](s: Set[T], op:(T,T)=>T):Boolean = {
    s.forall(a =>
               s.forall(b =>
                          op(a,b) == op(b,a)))
  }

  // TASK: Given a set and binary operation, detect exhaustively
  //   whether it is a ring.  I.e., check all possible cases
  //   of the ring axioms.  make use of the isGroup, isAbelian, and isMonoid
  //   functions.
  def isRing[T](s: Set[T], add: (T, T) => T, times: (T,T)=>T): Boolean = {
    isGroup(s,add) &&
      isAbelian(s,add) &&
      isMonoid(s,times) &&
      s.forall(a =>
               s.forall(b =>
                        s.forall(c => times(a,add(b,c)) == add(times(a,b),times(a,c)))))
  }

  // TASK: Given a set and binary operation, detect exhaustively
  //   whether it is a field.  I.e., check all possible cases
  //   of the field axioms.  make use of the isRing, isAbelian, and isGroup
  //   functions.   Be careful, the set is not necessarily a group,
  //   you must test group-ness on the set minus the zero element, which
  //   you must find using the findIdentity function.
  def isField[T](s: Set[T], add: (T, T) => T, times: (T,T)=>T): Boolean = {
    isAbelian(s,times) &&
      isRing(s,add,times) && {
      val Some(zero) = findIdentity(s,add)
      isGroup(s - zero, times)
    }
  }
}
