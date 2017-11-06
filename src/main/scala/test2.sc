// #1

// your code goes here
val sLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"
val str = "GHMABGZ VKXTMXL LNVVXLL EBDX GHG-LMHI, XGMANLBTLMBV XYYHKM"
val lLetters = sLetters.toList
val maxShift = lLetters.length / 2 - 1
val ind =lLetters.indexOf('A', maxShift)
val shifts =1 to maxShift
def transf( c: Char, shift: Int): Char = {
  val i = lLetters.indexOf(c, maxShift)
  if (i != -1)  lLetters(i - shift)
  else  c
}
val resAll = for (shift <- shifts) yield
  for (l <- str) yield transf(l, shift)

val result1 = "NOTHING CREATES SUCCESS LIKE NON-STOP, ENTHUSIASTIC EFFORT"

println(s"#1 - $result1")

// #2

// your code goes here

val a = "10001011101010101010000111110111011110101010101101110101010101010010000010110100101010101011011010100101011010101010101010101010101110101011000101101011110101010101010101010001010101010101101010101010101010101010101010111000001010101111010100111010101001011101010111111111101010101111111101010111110101001010101111110111101011010111111101011110101111111111111101111111111010101111101010101001111101010101010100100101010111101001010101001010101001010111110101010101010101011110101010010101001111101010100101111101010101001111111111101010111111111101001010111111110110101001111101010101111111010110100011111111111010101101011111110101010101110101010101010001110111101010101010101010101000001010110111111010101010010101011110101010000001010101000000000000101001111100000000000010010101010000001"
val b = "11100101000010101000001010010000010101011000110000110101000001010100000010000000010101100000110100100010111111111111111010010001010000001000000100000101011110101000000001010100000001010100101010111001010100000000000010101010101101010010101010101111001010000000000000001010010100111000010000000010100001010101000000110000001010101000000000000101001111100000000000010010101010000001"

def binSumRever(x :List[Int], y: List[Int], c: Int) : List[Int] = (x,y,c) match
{
  case (Nil, Nil, 1) => List(1)
  case (Nil, Nil, 0) => Nil
  case (Nil, ys, 0) => ys
  case (xs, Nil, 0) => xs
  case (Nil, ys, 1) => binSumRever(List(1),ys,0)
  case (xs, Nil, 1) => binSumRever(xs,List(1),0)
  case (x::xs, 0::ys, 0) => x::binSumRever(xs,ys,0)
  case (0::xs, y::ys, 0) => y::binSumRever(xs,ys,0)
  case (0::xs, 0::ys, 1) => 1::binSumRever(xs,ys,0)
  case (1::xs, 0::ys, 1) => 0::binSumRever(xs,ys,1)
  case (0::xs, 1::ys, 1) => 0::binSumRever(xs,ys,1)
  case (1::xs, 1::ys, 1) => 1::binSumRever(xs,ys,1)
  case (1::xs, 1::ys, 0) => 0::binSumRever(xs,ys,1)
}

def binarySum(first :String, second: String) :String =
{
  val firstReverse = (for (c <- first.toList) yield c.asDigit).reverse
  val secondReverse = (for (c <- second.toList) yield c.asDigit).reverse
  binSumRever(firstReverse,secondReverse,0).reverse.mkString("")
}

val rez = binarySum(a,b)
val one = rez.count(_ == '1')
val zero = rez.count(_ == '0')
val rez2 = one - zero

println(s"#2 - $rez2")

// #3
def isBinaryPalindrom (i :Int):Boolean ={
  val str = i.toBinaryString
  val len = str.length
  for (i <- 0 until len/2) if (str(i) != str(len-i-1)) return false
  return true
}

val n = 73
val rez3 = (for (i <- 1 to 10000 if  isBinaryPalindrom(i)) yield i).take(n).sum


// your code goes here

println(s"#3 - $rez3")

// #4

// your code goes here
val  x = Array (-1, -1, -2, -2, 1, -5, 1, 0, 1, 14, -8, 4, 5, -11, 13, 5, 7, -10, -4, 3, -6, 8, 6, 2, -9, -1, -4, 0)
val range = 0 until x.length
val list = (for (a <- range;
                 b<-range;
                 c<-range
                 if (a!=b)&&(a!=c)&&(b!=c)&&(x(a)+x(b)+x(c) == 0))
  yield List(x(a),x(b),x(c)).sorted).toList.distinct
val rez4 = list.length

println(s"#4 - $rez4")

// #5

// your code goes here
import scala.io.Source
val n1=10
val listOfLines = Source.fromFile("/home/inoquea/Documents/projects/test2/task5.txt").getLines.toList
val listOfBigInt = listOfLines map (BigInt(_))
val sum = listOfBigInt.foldLeft(BigInt(0))(_+_).toString.take(n1)

println(s"#5 - $sum")