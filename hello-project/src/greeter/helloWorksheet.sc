package greeter

object helloWorksheet {
val ss: List[(Char, Int)] = List(('a', 2))        //> ss  : List[(Char, Int)] = List((a,2))
  val l: List[(Char, Int)] = List(('a', 2), ('b', 2))
                                                  //> l  : List[(Char, Int)] = List((a,2), (b,2))
  
  def func( t: (Char, Int) ) =
  	(for (i <- 1 to t._2) yield (t._1, i)).toList
                                                  //> func: (t: (Char, Int))List[(Char, Int)]
  	
  val hard = for (s <- l) yield (func(s))         //> hard  : List[List[(Char, Int)]] = List(List((a,1), (a,2)), List((b,1), (b,2)
                                                  //| ))
  
                                                  
  val l1 = List(List(1, 2), List(3, 4))           //> l1  : List[List[Int]] = List(List(1, 2), List(3, 4))
  val l2 = List(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
                                                  //> l2  : List[List[Int]] = List(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
                                                  //| 
  
  val l3 = List(1, 2)                             //> l3  : List[Int] = List(1, 2)
  val l4 = List(3, 4)                             //> l4  : List[Int] = List(3, 4)
  
  for {
  	x <- l3
  	y <- l4
  } yield (x, y)                                  //> res0: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))
  
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
  }                                               //> test: (list: List[List[Int]])Unit
  
  test(l1)                                        //> 1 2 3 4 
  
  def test2(list: List[List[Int]], partialAcc: List[Int], acc: List[List[Int]]): List[List[Int]] = list match {
  	case List() => partialAcc :: acc
  	case head :: tail => head match {
  		case List() => test2(tail, partialAcc, acc)
  		case h :: t => test2(tail, partialAcc ++ List(h), test2(t :: tail, partialAcc, acc))
  	}
  }                                               //> test2: (list: List[List[Int]], partialAcc: List[Int], acc: List[List[Int]])
                                                  //| List[List[Int]]
  
  test2(l1, List(), List())                       //> res1: List[List[Int]] = List(List(1, 3), List(1, 4), List(1), List(2, 3), L
                                                  //| ist(2, 4), List(2), List(3), List(4), List())
 
 	List(List(1), List(List(2), List(3)), List(List(4, 5), List(6), List(7))).flatten
                                                  //> res2: List[Any] = List(1, List(2), List(3), List(4, 5), List(6), List(7))
	val la: List[Int] = List()                //> la  : List[Int] = List()
	val lb: List[List[Int]] = List()          //> lb  : List[List[Int]] = List()
	la :: lb                                  //> res3: List[List[Int]] = List(List())
}