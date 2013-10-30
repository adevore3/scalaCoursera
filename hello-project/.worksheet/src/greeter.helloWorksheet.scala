package greeter

object helloWorksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(121); 
  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ++ (xs drop n + 1);System.out.println("""removeAt: [T](n: Int, xs: List[T])List[T]""");$skip(27); 
  
  val a = List(1, 2, 3);System.out.println("""a  : List[Int] = """ + $show(a ));$skip(30); 
  val b = List('a', 'b', 'c');System.out.println("""b  : List[Char] = """ + $show(b ));$skip(20); val res$0 = 
  
  removeAt(0, a);System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(17); val res$1 = 
  removeAt(1, b);System.out.println("""res1: List[Char] = """ + $show(res$1));$skip(18); val res$2 = 
  removeAt(-1, a);System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(17); val res$3 = 
  removeAt(2, a);System.out.println("""res3: List[Int] = """ + $show(res$3));$skip(17); val res$4 = 
  removeAt(4, b);System.out.println("""res4: List[Char] = """ + $show(res$4));$skip(121); 
  
  def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
  	(for (x <- xs; y <- ys) yield (x * y)).sum
  };System.out.println("""scalarProduct: (xs: List[Double], ys: List[Double])Double""");$skip(42); 
  
  val xs: List[Double] = List(1, 3, 5);System.out.println("""xs  : List[Double] = """ + $show(xs ));$skip(39); 
  val ys: List[Double] = List(2, 4, 6);System.out.println("""ys  : List[Double] = """ + $show(ys ));$skip(36); 
  
  val sp = scalarProduct(xs, ys);System.out.println("""sp  : Double = """ + $show(sp ));$skip(34); 
  
  val p = List((1, 2), (3, 4));System.out.println("""p  : List[(Int, Int)] = """ + $show(p ));$skip(15); val res$5 = 
  
  'c' < 'd';System.out.println("""res5: Boolean(true) = """ + $show(res$5));$skip(48); 
  
  val ss: List[(Char, Int)] = List(('a', 2));System.out.println("""ss  : List[(Char, Int)] = """ + $show(ss ));$skip(54); 
  val l: List[(Char, Int)] = List(('a', 2), ('b', 2));System.out.println("""l  : List[(Char, Int)] = """ + $show(l ));$skip(83); 
  
  def func( t: (Char, Int) ) =
  	(for (i <- 0 to t._2) yield (t._1, i)).toList;System.out.println("""func: (t: (Char, Int))List[(Char, Int)]""");$skip(46); 
  	
  val hard = for (s <- l) yield (func(s));System.out.println("""hard  : List[List[(Char, Int)]] = """ + $show(hard ));$skip(202); 
  
  def aux(ll: List[List[(Char, Int)]]): List[(Char, Int)] = ll match {
		case List() => List()
		case head :: tail => head match {
			case List() => aux(tail)
			case h :: t => h :: aux(tail)
		}
	};System.out.println("""aux: (ll: List[List[(Char, Int)]])List[(Char, Int)]""");$skip(299); 
  
	def matches(listLeft: List[List[(Char,Int)]], acc: List[List[(Char, Int)]]): List[List[(Char, Int)]] = {
		listLeft match {
			case List() => acc
			case head :: tail => head match {
				case List() => matches(tail, acc)
				case t :: tt => matches(tt :: tail, aux(listLeft) :: acc)
			}
		}
	};System.out.println("""matches: (listLeft: List[List[(Char, Int)]], acc: List[List[(Char, Int)]])List[List[(Char, Int)]]""");$skip(91); 
                                                  
  val l1 = List(List(1, 2), List(3, 4));System.out.println("""l1  : List[List[Int]] = """ + $show(l1 ));$skip(52); 
  val l2 = List(List(3), List(4), List(6), List(8));System.out.println("""l2  : List[List[Int]] = """ + $show(l2 ))}
  
  
	
		
  
  /*val result: List[List[(Char, Int)]] = {
  	s.map((c,n) => (for (i <- 0 to n) yield (c, i)).toList
  }*/
}
