package algebra

object AbstractAlgebraFinite {
  // TASK: determine whether the given element, e, is actually
  //   the identity element for the set under the operation.
  def isIdentity[T](e: T, s: Set[T], op: (T, T) => T): Boolean = {
    s.forall { x => // e + x == x + e == x
      op(e, x) == x &&
      op(x, e) == x
    }
  }

  // TASK: search the set s for an identity element,
  //    return None if no such element exists
  //    otherwise, find e such that e is the identity,
  //    then return Some(e).  This function should make use of
  //    the isIdentity function.
  def findIdentity[T](s: Set[T], op: (T, T) => T): Option[T] = {
    // The identity is guaranteed to be unique if it exists.
    // Because if there are two identifies e1 and e2, then
    // op(e1,e2)=e1 and op(e1,e2)=e2, by definition of identity.
    // so e1=e2.
    s.find{x => isIdentity(x, s, op)}

  }

  def hasIdentity[T](s: Set[T], op: (T, T) => T): Boolean = {
    // Boolean to indicate whether or not the set has an identity
    // element.
    findIdentity(s, op) != None
  }

  // TASK:  Is the given set closed under the given operation?
  //   I.e., is it true that for ever x and y in s, that
  //   op(x,y) is also in s?
  def isClosed[T](s: Set[T], op: (T, T) => T): Boolean = {
    s.forall(a => 
     s.forall(b =>
       s.contains(op(a, b))))
  }

  // TASK: is it true that for all a, b, and c in s, that
  //   op(op(a, b), c) == op(a, op(b, c))?
  def isAssociative[T](s: Set[T], op: (T, T) => T): Boolean = {
    s.forall { a =>
      s.forall { b =>
        s.forall { c =>
          op(op(a, b), c) == op(a, op(b, c))
        }
      }
    }
  }
  // Given a set and binary operation, detect exhaustively
  //   whether it is a monoid.  I.e., check all possible cases
  //   of the monoid axioms.
  def isMonoid[T](s: Set[T], op: (T, T) => T): Boolean = {
    isClosed(s, op) &&
    isAssociative(s, op) &&
    hasIdentity(s, op)
  }

  // TASK: Given a set and binary operation, detect exhaustively
  //   whether it is a group.  I.e., check all possible cases
  //   of the group axioms.   Make use of the isMonoid function.
  def isGroup[T](s: Set[T], op: (T, T) => T): Boolean = {
    // is monoid?
    // every element has identity

    isMonoid(s, op) && {
      val id = findIdentity(s, op).get
      s.forall { x => s.exists { y => op(x, y) == id } }
    }
  }

  // TASK: Given a set and binary operation, detect exhaustively
  //   whether the operation is commutative
  def isAbelian[T](s: Set[T], op: (T, T) => T): Boolean = {
    s.forall { x =>
      s.forall { y =>
        op(x, y) == op(y, x)
      }
    }
  }

  // TASK: Given a set and binary operation, detect exhaustively
  //   whether it is a ring.  I.e., check all possible cases
  //   of the ring axioms.  Make use of the isGroup, isAbelian,
  //   and isMonoid functions.
  def isRing[T](s: Set[T], add: (T, T) => T, times: (T, T) => T): Boolean = {
    // does addition make an Abelian group
    // does times make a monoid
    // do (right and left) distributive laws work for ALL elements
    if (isAbelian(s, add) == false)
     false
    else if (isMonoid(s, times) == false)
     false
    else
       (s.forall {
          x => s.forall{
            y => s.forall{
              z => times(x, add(y,z)) == add(times(x, y), times(x, z)) &&
                   times(add(y,z), x) == add(times(y, x), times(z, x))
            }
          }
       }
      ) 
    }

  // TASK: Given a set and binary operation, detect exhaustively
  //   whether it is a field.  I.e., check all possible cases
  //   of the field axioms.  Make use of the isRing, isAbelian, and isGroup
  //   functions.   Be careful, the set is not necessarily a group,
  //   you must test group-ness on the set minus the zero element, which
  //   you must find using the findIdentity function.
  def isField[T](s: Set[T], add: (T, T) => T, times: (T, T) => T): Boolean = {
    // Is times commutative?
    // Is s ring using add and times?
    // Is (s, add, times) a group if we exclude the zero (additive identity)?
    if (!(hasIdentity(s, add)))
      false
    else {
      val id = findIdentity(s, add).get
      val neogroup = s - id
      isRing(s, add, times) &&
      isAbelian(s, times) &&
      isGroup(neogroup, times) &&
      isAbelian(neogroup, times)
    }
  }
}
