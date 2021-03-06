package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1") = forAll { (e1: Int, e2: Int) =>
    val heap = insert(e1, empty)
    findMin(insert(e2, heap)) == min(e1, e2)
  }

  property("hint2") = forAll { (e1: Int) =>
    val heap = insert(e1, empty)
    val emptyHeap = deleteMin(heap)
    isEmpty(emptyHeap)
  }

  property("hint3") = forAll { (h: H) =>
    def isOrdered(h: H, l: List[A]): Boolean = {
      if (isEmpty(h)) true
      else {
        val element = findMin(h)
        if (!l.isEmpty && l.max > element) false
        else isOrdered(deleteMin(h), element :: l)
      }
    }
    isOrdered(h, List())
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
