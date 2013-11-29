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
  
  test("times tests") {
    assert(times("ababababa".toList) === List(('a', 5), ('b', 4)))
    assert(times("ggggccceeeee".toList) === List(('g', 4), ('c', 3), ('e', 5)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
    assert(makeOrderedLeafList(List(('t', 1), ('e', 2), ('x', 3))) === List(Leaf('t',1), Leaf('e',2), Leaf('x',3)))
  }
  
  test("singleton tests") {
    new TestTrees {
      assert(!singleton(List()))
      assert( singleton(List(Leaf('x', 4))))
      assert(!singleton(List(Leaf('x', 3), Leaf('y', 4))))
      assert( singleton(List(t1)))
      assert(!singleton(List(t1, t2, Leaf('x', 4))))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      println(decodedSecret.mkString)
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
      assert(decode(t2, quickEncode(t2)("abd".toList)) === "abd".toList)
    }
  }
  
  test("more serious tests on createCodeTree, encoding and decoding") {
    def test(s: String, enf: CodeTree => (List[Char] => List[Bit])) {
      val ls = s.toList
      val tree = createCodeTree(ls)
      assert(decode(tree, enf(tree)(ls)) === ls)
    }
    val s = "decode and encode a very short text should be identity"
    test(s, encode); test(s, quickEncode)
    val t = "In a normal, uncompressed text, each character is represented by the same number of bits (usually eight). In Huffman coding, each character can have a bit representation of a different length, depending on how common a character is: the characters that appear often in a text are represented by a shorter bit sequence than those being used more rarely. Every huffman code defines the specific bit sequences used to represent each character."
    test(t, encode); test(t, quickEncode)
  }
}
