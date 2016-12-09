package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("times on list of \"hoollo\"") {
    assert(times(string2Chars("hoollo")) === List(('o', 3), ('l', 2), ('h', 1)))
  }

  test("singleton on list of two") {
    assert(singleton(List(Leaf('e',1), Leaf('t',2))) === false)
  }

  test("singleton on list of one") {
    assert(singleton(List(Leaf('e',1))) === true)
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until of some leaf list") {
	val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
	val resultList = until(singleton, combine)(leaflist)
	val expectedList = List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t','x'), 7))
	assert(resultList === expectedList)
  }

  test("createCodeTree on some text"){
	  val expectedTree = Fork(Leaf('b',1), Leaf('a',2), List('b', 'a'), 3)
	  assert(createCodeTree("aba".toList) === expectedTree)
  }

  test("decode"){
	  val tree = Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t','x'), 7)
	  val bits = List(1,1,0,1,0,0,1,0,1,1)
	  assert(decode(tree, bits) === "xxtextx".toList)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a short text should be identity") {
    new TestTrees {
  	assert(decode(t2, encode(t2)("abaadbd".toList)) === "abaadbd".toList)
    }
  }

  test("codeBits") {
	 val table = List(('a', List(0)), ('b', List(1)))
	 assert(codeBits(table)('b') === List(1))
  }

  test("convert") {
	  new TestTrees {
		assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
	  }
  }

  test("decode and quickEncode a short text should be identity") {
    new TestTrees {
    assert(decode(t2, quickEncode(t2)("abaadbd".toList)) === "abaadbd".toList)
    }
  }

}
