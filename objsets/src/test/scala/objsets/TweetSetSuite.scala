package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    
    val s = new Empty
    val sg = s.incl(new Tweet("g", "g", 40))
    val scg = sg.incl(new Tweet("c", "c", 20))
    val sceg = scg.incl(new Tweet("e", "e", 30))
    val saceg = sceg.incl(new Tweet("a", "a", 10))
    val sacegk = saceg.incl(new Tweet("k", "k", 60))
    val sacegik = sacegk.incl(new Tweet("i", "i", 50))
    val sacegikm = sacegik.incl(new Tweet("m", "m", 70))
    
    val sh = s.incl(new Tweet("h", "h", 110))
    val sdh = sh.incl(new Tweet("d", "d", 90))
    val sdfh = sdh.incl(new Tweet("f", "f", 100))
    val sbdfh = sdfh.incl(new Tweet("b", "b", 80))
    val sbdfhl = sbdfh.incl(new Tweet("l", "l", 130))
    val sbdfhjl = sbdfhl.incl(new Tweet("j", "j", 120))
    val sbdfhjln = sbdfhjl.incl(new Tweet("n", "n", 140))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  
  test("union with full trees") {
    new TestSets {
      val bigUnion = sacegikm.union(sbdfhjln)
      assert(size(bigUnion) === 14)
//      bigUnion foreach println
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
}
