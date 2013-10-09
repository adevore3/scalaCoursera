package objsets

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(272); 
  def p(list: List[String]) = { tweet: Tweet =>
	def aux(l: List[String]): Boolean = {
	  l match {
	    case head::tail => {
	      if (tweet.text.contains(head)) true
	      else aux(tail)
	    }
	    case _ => false
	  }
	}
	aux(list)
  };System.out.println("""p: (list: List[String])objsets.Tweet => Boolean""");$skip(83); 
  
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus");System.out.println("""google  : List[String] = """ + $show(google ));$skip(74); 
                                                  
  val set1 = new Empty;System.out.println("""set1  : objsets.Empty = """ + $show(set1 ));$skip(56); 
    val set2 = set1.incl(new Tweet("a", "android", 20));System.out.println("""set2  : objsets.TweetSet = """ + $show(set2 ));$skip(55); 
    val set3 = set2.incl(new Tweet("b", "galaxy", 30));System.out.println("""set3  : objsets.TweetSet = """ + $show(set3 ));$skip(57); 
    val set4 = set3.incl(new Tweet("c", "Androidp", 20));System.out.println("""set4  : objsets.TweetSet = """ + $show(set4 ));$skip(68); 
    
    val tweetList = set4.filter(p(google)).descendingByRetweet;System.out.println("""tweetList  : objsets.TweetList = """ + $show(tweetList ));$skip(47); 
    
    tweetList.foreach { e => println(e) }}
    
    
}
