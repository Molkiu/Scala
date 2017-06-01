def last[A](x: List[A]):A = {
  if (x.tail.isEmpty) x.head
  else last(x.tail)
}

def penultimate[A](x: List[A]): A ={
  if(x.length == 2) x.head
  else penultimate(x.tail)
}

def nth[A](x:Int, y: List[A]): A ={
  if(x== 0) y.head
  else nth(x-1, y.tail)
}

def length[A](x: List[A]): Int={
  def loop(acc: Int, y:List[A]): Int ={
    if(y.isEmpty) acc
    else loop (acc+1, y.tail)
  }
  loop(0,x)
}

def reverse[A](x: List[A]):List[A]={
  def loop(acc: List[A], y:List[A]): List[A] ={
    if(y.isEmpty) acc
    else loop (acc :+y.last, y.init)
  }
  loop(List(),x)
}

def isPalindrome[A](x: List[A]):Boolean={
  val y = reverse(x)
  x.zip(y).forall{
    case(x,y) => x==y
  }
}

