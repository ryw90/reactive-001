package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("findMin finds the minmum of 1-heap") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin find the minimum of 2-heap") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == a.min(b)
  }

  property("insert then deleteMin is empty") = forAll{ (a: Int) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    h2 == empty
  }

  property("insert of findMin then deleteMin does nothing") = forAll{ h: H =>
    val min = findMin(h)
    val h2 = insert(min, deleteMin(h))
    min == findMin(h2)
  }

  property("elements are sorted") = forAll { h: H =>
    val elems = heapElems(h)
    elems == elems.sorted
  }

  property("elements are sorted after meld") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    (heapElems(h1) ++ heapElems(h2)).sorted == heapElems(h)
  }

  property("min of melded is min of mins") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    findMin(h) == (findMin(h1) min findMin(h2))
  }

  def heapElems(h: H): List[Int] =
    if(isEmpty(h)) Nil else findMin(h) :: heapElems(deleteMin(h))

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(empty, genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
