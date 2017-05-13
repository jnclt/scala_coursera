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

}
