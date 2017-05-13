package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  val heapGen = for {
      elements <- Gen.listOf[A](arbitrary[A])
  } yield insertElements(elements, empty)

  def insertElements(elements: List[A], heap: H): H = elements match {
      case Nil => heap
      case e::xs => insertElements(xs, insert(e, heap))
  }

  property("gen1") = forAll(heapGen) { h =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minimum is the smaller of two") = forAll { (e1: A, e2: A) =>
      findMin(insert(e2, insert(e1, empty))) == (if (e1 < e2) e1 else e2)
  }

  property("insert and deleteMin on empty heap gives empty heap") = forAll { (e: A) =>
      isEmpty(deleteMin(insert(e, empty)))
  }

  property("deleting mins gives a non-decreasing sequence") = forAll(heapGen) { h =>
      isNonDecreasing(getSequenceOfAllMins(h))
  }

  def getSequenceOfAllMins(heap: H): List[A] = {
      if (isEmpty(heap)) Nil
      else findMin(heap)::getSequenceOfAllMins(deleteMin(heap))
  }

  def isNonDecreasing(sequence: List[A]): Boolean = sequence match {
      case Nil => true
      case x::Nil => true
      case x1::x2::xs => (x1 <= x2) && isNonDecreasing(x2::xs)
  }

  property("min of melded heap is min of original heap") = forAll(heapGen, heapGen) { (h1, h2) =>
      val melded = meld(h1, h2)
      if (!isEmpty(melded)) {
          val minMelded = findMin(melded)
          val minH1 = if (isEmpty(h1)) Nil else findMin(h1)
          val minH2 = if (isEmpty(h2)) Nil else findMin(h2)
          minMelded == minH1 || minMelded == minH2
      }
      else true
  }


}
