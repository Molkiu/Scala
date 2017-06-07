import scala.annotation.tailrec

def isPrime(n:Int):Boolean= (2 until n) forall (x => n%x !=0)

def nextPrime(p:Int):Int={
  if(isPrime(p+1)) p+1
  else nextPrime(p+1)
}

def gcd(n:Int,m:Int):Int= if (m == 0) n else gcd(m, n % m)

def isCoprime(n:Int,m:Int):Boolean= gcd(n,m)==1

def totient(n:Int):Int = (1 to n) count (x => n%x==0)

def primeFactors(n:Int):List[Int]={
  @tailrec
  def loop(acc:List[Int]=Nil,prime:Int=2,y:Int=n):List[Int]= prime*prime > y match{
    case false if y%prime==0 => loop(prime::acc,prime,y/prime)
    case false => loop(acc,prime+1,y)
    case true => y::acc
  }
  loop().reverse
}

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}
/* slower
More or less 'Scala version' of this clojure function (by me in recursion.clj)

(defn my-frequencies-helper [freqs a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (update-in freqs [head] (fnil inc 0)) tail))))

def frequencies2[T](ss:Seq[T]):Map[T,Int]={
  def loop(freq:Map[T,Int]=Map[T,Int](),seq:Seq[T]=ss):Map[T,Int]={
    if(seq.isEmpty) freq
    else loop(freq.updated(seq.head, seq count(_ == seq.head)), seq filterNot(_ == seq.head))
  }
  loop()
}
val x = Seq(2,3,2,6,2)
time(frequencies(x))
time(frequencies2(x))
*/
def frequencies[T](ss:Seq[T]):Map[T,Int]={
  ss.groupBy(y => y) map (a =>(a._1,a._2.length))
}

def primeFactorMultiplicity(n:Int):Map[Int,Int]={
  frequencies(primeFactors(n))
}

def listPrimesInRange(lo:Int,hi:Int):List[Int]={
  def loop(acc:List[Int],l:Int=lo):List[Int]={
    if(nextPrime(l)>hi)acc
    else loop(acc:+nextPrime(l),nextPrime(l))
  }
  if(isPrime(lo))loop(List(lo))
  else loop(Nil)
}
def goldbach(n:Int):(Int,Int)={
  val y = listPrimesInRange(2,n)
  val h = y filter (x => y.contains(n - x))
  if(h.isEmpty) throw new IllegalArgumentException else (h.head,h.last)
}

def printGoldbachList(r:Range)={
  r filter(_%2==0) foreach{x=>println(x+" = "+goldbach(x)._1+" + "+goldbach(x)._2)}
}



