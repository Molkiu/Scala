import scala.annotation.tailrec

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

