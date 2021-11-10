package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = Gen.oneOf(
  Gen.const(empty), 
  for
    k <- arbitrary[Int]
    h <- Gen.oneOf(Gen.const(empty), genHeap)
  yield insert(k, h)
)
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin from a heap of two elements should yield min of these two elements") = forAll { (i: Int, j : Int) =>
    val e = insert(j, insert(i, empty))
    val m = findMin(e)
    m == (i).min(j)
  }

  property("insert one, deleteMin => empty") = forAll { (i: Int) =>
    val h = insert(i, empty)
    val h0 = deleteMin(h)
    h0 == empty
  }

  property("one element heap, findMin should yield this only element") = forAll { (i : Int) =>
    val h = insert(i, empty)
    findMin(h) == i  
  }

  property("iteratively findMin and deleteMin => ascending sequence") = forAll { (h : H) =>
    val a = toList(h)
    if (a.size==0) then true else a.lazyZip(a.tail).forall(_ <= _) 
  }

  property("iteratively findMin and deleteMin => ascending sequence") = forAll { (h : H) =>
    val a = toList(h)
    if (a.size==0) then true else a.lazyZip(a.tail).forall(_ <= _) 
  }
  
  property("min meld two heaps == meld min one heap") = forAll { (h1: H, h2: H) => if ((h1==empty)||(h2==empty)) then true else findMin(meld(h1, h2)) == (findMin(h1)).min(findMin(h2))
  }

  property("Commutativity of meld") = forAll { (h1: H, h2: H) => toList(meld(h1, h2)) == toList(meld(h2, h1))}

  property("Associativity of meld") = forAll { (h1: H, h2: H, h3: H) => toList(meld(h1, meld(h2, h3))) == toList(meld(meld(h1, h2), h3))}

  private def toList(h : H): List[Int] = if (h==empty) then Nil else findMin(h) :: toList(deleteMin(h))
