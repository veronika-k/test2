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

val ress1 = "NOTHING CREATES SUCCESS LIKE NON-STOP, ENTHUSIASTIC EFFORT"

println(s"#1 - $ress1")

// #2

// your code goes here

val a = "10001011101010101010000111110111011110101010101101110101010101010010000010110100101010101011011010100101011010101010101010101010101110101011000101101011110101010101010101010001010101010101101010101010101010101010101010111000001010101111010100111010101001011101010111111111101010101111111101010111110101001010101111110111101011010111111101011110101111111111111101111111111010101111101010101001111101010101010100100101010111101001010101001010101001010111110101010101010101011110101010010101001111101010100101111101010101001111111111101010111111111101001010111111110110101001111101010101111111010110100011111111111010101101011111110101010101110101010101010001110111101010101010101010101000001010110111111010101010010101011110101010000001010101000000000000101001111100000000000010010101010000001"
val b = "11100101000010101000001010010000010101011000110000110101000001010100000010000000010101100000110100100010111111111111111010010001010000001000000100000101011110101000000001010100000001010100101010111001010100000000000010101010101101010010101010101111001010000000000000001010010100111000010000000010100001010101000000110000001010101000000000000101001111100000000000010010101010000001"
//val a = "100001"
//val b = "1"
val la =a.length
val lb =b.length


val sum = new StringBuilder
val min = if (la > lb) lb else la
val max =  if (la > lb) la else lb
var carry = 0
def addbin(i:Int): Int =
{
  val m = if (la - i - 1 < 0) 0 else a.charAt(la - i - 1).toInt - 48
  val n = if (lb - i - 1 < 0) 0 else b.charAt(lb - i - 1).toInt - 48
  val add = m + n + carry
  carry = add/2
  add % 2
}

val rez = (for(i <- (0 until max)) yield addbin(i)).reverse

val one = rez.count(_ == 1)
val zero = rez.count(_ == 0)
val ress2 = one - zero

println(s"#2 - $ress2")

// #3
var count = 0
var maximum = 73
def isPalindrom (str: String):Boolean ={
  val len = str.length
  for (i <- 0 until len/2) if (str(i) != str(len-i-1)) return false
  return true
}
val listForSum = for (i <- 0 until 1350 if isPalindrom(i.toBinaryString) ) yield i.toBinaryString
//toList.mkString("")
//val summAll =
val length = listForSum.length

//def add3(a:String, b:String):String =
//{
//  def addbin2(i:Int): Int =
//  {
 //   val m = if (la - i - 1 < 0) 0 else a.charAt(la - i - 1).toInt - 48
//    val n = if (lb - i - 1 < 0) 0 else b.charAt(lb - i - 1).toInt - 48
 //   val add = m + n + carry
 //   carry = add/2
 //   add % 2
 // }
 // ((for(i <- (0 until max)) yield addbin(i)).reverse).toList.mkString("")
//}
// val rez3 = listForSum. FoldLeft("")(add3)



// your code goes here

println(s"#3 - ${/*answer #3*/}")

// #4

// your code goes here

println(s"#4 - ${/*answer #4*/}")

// #5

// your code goes here

println(s"#5 - ${/*answer #5*/}")