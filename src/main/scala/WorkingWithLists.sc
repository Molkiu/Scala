import scala.annotation.tailrec
import math.Ordering

def last[A](xs: List[A]):A = {
  if (xs.tail.isEmpty) xs.head
  else last(xs.tail)
}

def penultimate[A](xs: List[A]): A ={
  if(xs.length == 2) xs.head
  else penultimate(xs.tail)
}

def nth[A](x:Int, ys: List[A]): A ={
  if(x== 0) ys.head
  else nth(x-1, ys.tail)
}

def length[A](xs: List[A]): Int={
  @tailrec
  def loop(acc: Int, ys:List[A]): Int ={
    if(ys.isEmpty) acc
    else loop (acc+1, ys.tail)
  }
  loop(0,xs)
}

def reverse[A](xs: List[A]):List[A]={
  @tailrec
  def loop(acc: List[A], ys:List[A]): List[A] ={
    if(ys.isEmpty) acc
    else loop (acc :+ys.last, ys.init)
  }
  loop(List(),xs)
}

def isPalindrome[A](xs: List[A]):Boolean={
  val y = reverse(xs)
  xs.zip(y).forall{
    case(xs,y) => xs==y
  }
}

def flatten(xs: List[Any]): List[Any]= xs flatMap{
  case i : List[_] => flatten(i)
  case e => List(e)
}

def compress[A](ls: List[A]):List[A]={
  @tailrec
  def loop(acc: List[A], ys:List[A]): List[A] = ys match{
    case Nil => acc
    case a::b::ps if a == b => loop(acc,ys.tail)
    case _ => loop(acc:+ys.head,ys.tail)
  }
  loop(List(),ls)
}

def pack[A](ls: List[A]):List[List[A]]={
  @tailrec
  def loop(acc: List[List[A]], ys:List[A]): List[List[A]] = ys match{
    case Nil => acc
    case xs => {
      val (x: List[A], y: List[A]) = ys span {_ == ys.head}
      loop(acc ::: List(x), y)
    }
  }
  loop(List(),ls)
}

def encode[A](ls:List[A]):List[(Int,A)]={
  pack(ls) map {x => (x.length,x.head)}
}

def encodeModified[A](ls:List[A]):List[Any]={
  encode(ls) map (x =>if (x._1 == 1) x._2 else x)
}

def decode[A](ls: List[(Int,A)]):List[A]={
  ls flatMap (x =>List.fill(x._1)(x._2))
}

def encodeDirect[A](ls: List[A]):List[(Int,A)]={
  if(ls.isEmpty) Nil
  else{
    val (packed, next) = ls span {_ == ls.head}
    (packed.length,packed.head)::encodeDirect(next)
  }
}

def duplicate[A](ls: List[A]):List[A]={
  ls flatMap (x =>List.fill(2)(x))
}

def duplicateN[A](n: Int,ls: List[A]):List[A]={
  ls flatMap (x =>List.fill(n)(x))
}

def drop[A](n: Int, ls:List[A]):List[A]={
  (ls.grouped(n) flatMap (y => y.take(n-1))).toList
}

def split[A](n: Int, ls:List[A]):(List[A],List[A])={
  (ls take n, ls drop n)
}

def slice[A](n:Int,m:Int,ls:List[A]):List[A]={
  ls.drop(n).take(m-n)
}

def rotate[A](n:Int,ls: List[A]):List[A]={
  if (n <0) ((ls drop (length(ls)+n))::List(ls take (length(ls)+n))) flatten
  else ((ls drop n)::List(ls take n))flatten
}

def removeAt[A](n:Int, ls:List[A]):(List[A],A)={
  (((ls take n):: List(ls drop (n+1))) flatten,slice(n,n+1,ls).head)
}

def insertAt[A](x:A,n:Int,ls:List[A]):List[A]={
  ((ls take n)::List(x)::List(ls drop n)) flatten
}

def range(n:Int,m:Int):List[Int]={
  @tailrec
  def loop(acc: List[Int]=List(),a:Int=n):List[Int]={
    if(a==m)acc:+a
    else loop(acc:+a,a+1)
  }
  loop()
}

def randomSelect[A](n: Int,ls:List[A]):List[A]= {
  if (n >= ls.size) ls
  else {
  val rand = scala.util.Random

  def loop[A](acc: List[A] = List(), m: Int = n, xs: List[A] = ls): List[A] = {
    val x = rand.nextInt(xs.size)
    val res = removeAt(x, xs)
    if (m == 0) acc
    else loop(acc :+ res._2, m - 1, res._1)
  }

  loop()
}
}

def lotto(n:Int,m:Int):List[Int]={
  val rand = scala.util.Random
  @tailrec
  def loop(acc:List[Int]=List(),a:Int=n): List[Int] ={
    if(a==0)acc
    else loop(acc:+rand.nextInt(m),a-1)
  }
  loop()
}

def randomPermute[A](ls:List[A]):List[A]={
  ls.toSet.toList
}

def combinations[A](n:Int, ls:List[A]):List[List[A]]={
  @tailrec
  def loop(acc:List[List[A]],xs:List[A]=ls):List[List[A]]=xs match{
    case Nil => acc
    case a :: as => loop(acc :::(acc map (a ::_)),as)
  }
  loop(Nil::Nil).filter(x => x.size == n)
}

def group[A](xs:List[Int],ls:List[A]):List[List[List[Any]]]={
  List(combinations(nth(0,xs),ls)::combinations(nth(1,xs),ls)::combinations(nth(2,xs),ls))
}

