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
    val chars1 = "abbcccddddeeeee".toList
    val chars2 = "abcdebcdecdedee".toList
    val chars3 = chars1.reverse
    val chars4 = "ttxxexx".toList
    val expectedFrequencies = List(('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5))
    val expectedFrequencies2 = expectedFrequencies.reverse
    val orderedLeafList = List(Leaf('a', 1), Leaf('b', 2), Leaf('c', 3), Leaf('d', 4), Leaf('e', 5))
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val codeTree = Fork(Fork(Leaf('e',1),Leaf('t',2),List('e','t'),3),Leaf('x',4),List('e','t','x'),7)
    val codeTree2 = Fork(Fork(Leaf('c',3),Fork(Leaf('a',1),Leaf('b',2),List('a', 'b'),3),List('c', 'a', 'b'),6),Fork(Leaf('d',4),Leaf('e',5),List('d', 'e'),9),List('c', 'a', 'b', 'd', 'e'),15)
    val listCodeTree = List(codeTree)
    val listCodeTree2 = List(codeTree2)
    val bits = List(0,1,1, 0,1,0, 1,0, 1,0, 1,1, 0,1,0, 1,0, 0,0, 0,1,0, 0,1,1)
    val string1 = "baddeadcab".toList
    
    val a = List(0, 1, 0)
    val b = List(0, 1, 1)
    val c = List(0, 0)
    val d = List(1, 0)
    val e = List(1, 1)
    
    val codeTable = List(
      ('a', a),
      ('b', b),
      ('c', c),
      ('d', d),
      ('e', e))
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
  
  test("times of a large list of chars") {
    new TestTrees {
      assert(expectedFrequencies === times(chars1))
      assert(expectedFrequencies === times(chars2))
      assert(expectedFrequencies2 === times(chars3))
    }
  }
  
  test("makeOrderedLeafList properly generates a list of leafs in ascending order") {
    new TestTrees {
      assert(orderedLeafList === makeOrderedLeafList(expectedFrequencies))
      assert(orderedLeafList === makeOrderedLeafList(expectedFrequencies2))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("singleton returns true if lists contains one CodeTree") {
    new TestTrees {
      assert(singleton(List(t1)))
      assert(!singleton(List(t1, t2)))
    }
  }

  test("combine of some leaf list") {
    new TestTrees {
      assert(combine(leafList) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    }
  }
  
  test("until should return a single list of the appropriate CodeTree") {
    new TestTrees {
      assert(until(singleton,combine)(leafList) === listCodeTree)
      assert(until(singleton,combine)(orderedLeafList) === listCodeTree2)
    }
  }
  
  test("createCodeTree should create appropriate tree based on characters") {
    new TestTrees {
      assert(createCodeTree(chars4) === codeTree)
      assert(createCodeTree(chars3) === codeTree2)
    }
  }
  
  test("decode baddeadcab") {
    new TestTrees {
      assert(decode(codeTree2, bits) === string1)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(codeTree2, encode(codeTree2)(string1)) === string1)
    }
  }
  
  test("full integration test of decode, encode and createCodeTree") {
    new TestTrees {
      val s1 = "hello world, you will be missed".toList
      val s2 = "for one day, I shall rule them all".toList
      val s3 = "or just continue using scala for fun".toList
      for (s <- List(s1, s2, s3)) {
        val ct = createCodeTree(s)
        assert(decode(ct, encode(ct)(s)) === s)
      }
    }
  }
  
  test("codeBits returns correct bits") {
    new TestTrees {
	  for ((char, bits) <- List(('a', a), ('b', b), ('c', c), ('d', d), ('e', e))) {
	    assert(codeBits(codeTable)(char) === bits)
	  }
    }
  }
  
  test("quickEncode encodes strings properly") {
    new TestTrees {
      assert(quickEncode(codeTree2)(string1) === bits)
      assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
    }
  }
}
