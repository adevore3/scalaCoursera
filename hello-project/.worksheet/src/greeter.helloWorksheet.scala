package greeter

object helloWorksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(83); 
val ss: List[(Char, Int)] = List(('a', 2));System.out.println("""ss  : List[(Char, Int)] = """ + $show(ss ));$skip(54); 
  val l: List[(Char, Int)] = List(('a', 2), ('b', 2));System.out.println("""l  : List[(Char, Int)] = """ + $show(l ));$skip(83); 
  
  def func( t: (Char, Int) ) =
  	(for (i <- 1 to t._2) yield (t._1, i)).toList;System.out.println("""func: (t: (Char, Int))List[(Char, Int)]""");$skip(46); 
  	
  val hard = for (s <- l) yield (func(s));System.out.println("""hard  : List[List[(Char, Int)]] = """ + $show(hard ));$skip(94); 
  
                                                  
  val l1 = List(List(1, 2), List(3, 4));System.out.println("""l1  : List[List[Int]] = """ + $show(l1 ));$skip(64); 
  val l2 = List(List(1, 3), List(1, 4), List(2, 3), List(2, 4));System.out.println("""l2  : List[List[Int]] = """ + $show(l2 ));$skip(25); 
  
  val l3 = List(1, 2);System.out.println("""l3  : List[Int] = """ + $show(l3 ));$skip(22); 
  val l4 = List(3, 4);System.out.println("""l4  : List[Int] = """ + $show(l4 ));$skip(50); val res$0 = 
  
  for {
  	x <- l3
  	y <- l4
  } yield (x, y);System.out.println("""res0: List[(Int, Int)] = """ + $show(res$0));$skip(373); 
  
  def test(list: List[List[Int]]): Unit = list match {
  	case List() => println()
  	case head :: List() => head match {
  		case List() => println()
  		case h :: t => {
	  		print(h + " ")
	  		test(t :: List())
  		}
  	}
  	case head :: tail => head match {
  		case List() => test(tail)
  		case h :: t => {
  			print(h + " ")
  			test(t :: tail)
  		}
  	}
  };System.out.println("""test: (list: List[List[Int]])Unit""");$skip(14); 
  
  test(l1);$skip(334); 
  
  def test2(list: List[List[Int]], partialAcc: List[Int], acc: List[List[Int]]): List[List[Int]] = list match {
  	case List() => partialAcc :: acc
  	case head :: tail => head match {
  		case List() => test2(tail, partialAcc, acc)
  		case h :: t => test2(tail, partialAcc ++ List(h), test2(t :: tail, partialAcc, acc))
  	}
  };System.out.println("""test2: (list: List[List[Int]], partialAcc: List[Int], acc: List[List[Int]])List[List[Int]]""");$skip(31); val res$1 = 
  
  test2(l1, List(), List());System.out.println("""res1: List[List[Int]] = """ + $show(res$1));$skip(86); val res$2 = 
 
 	List(List(1), List(List(2), List(3)), List(List(4, 5), List(6), List(7))).flatten;System.out.println("""res2: List[Any] = """ + $show(res$2));$skip(28); 
	val la: List[Int] = List();System.out.println("""la  : List[Int] = """ + $show(la ));$skip(34); 
	val lb: List[List[Int]] = List();System.out.println("""lb  : List[List[Int]] = """ + $show(lb ));$skip(10); val res$3 = 
	la :: lb;System.out.println("""res3: List[List[Int]] = """ + $show(res$3))}
}
