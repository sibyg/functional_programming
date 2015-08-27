package week2

object Week2 extends App {
  def cube(x: Int) = x * x * x

  def square(x: Int) = x * x

  // example of tail recursion
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  // example of linear recursion
  def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

  // convert linear recursions into tail recursions using inner functions, thus use "free variables"; check free vs bound variables
  def tailRecursiveFact(x: Int): Int = {
    def fact(acc: Int, x: Int): Int = {
      if (x == 0) acc else fact(acc * x, x - 1)
    }
    fact(1, x)
  }

  def sumInts(a: Int, b: Int): Int = if (a > b) 0 else a + sumInts(a + 1, b)

  def sumFactorials(a: Int, b: Int): Int = if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)

  def linerRecursiveSum(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + linerRecursiveSum(f, a + 1, b)
  }

  def tailRecursiveSum(f: Int => Int, a: Int, b: Int): Int = {

    def loop(acc: Int, a: Int): Int = {
      if (a > b) acc else loop(acc + a, a + 1)
    }

    loop(0, a)
  }

  // returns function parameter
  def sum1(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = if (a > b) 0 else f(a) + sumF(a + 1, b)
    sumF
  }

  def sum1Ints = sum1(x => x)

  def sum1Square = sum1(x => x * x)

  def sum1Cube = sum1(x => x * x * x)

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product(f)(a + 1, b)
  }

  println(cube(3))
  println(square(12))
  println(gcd(48, 18))
  println(fact(4))
  println(tailRecursiveFact(4))
  println(tailRecursiveSum(square, 1, 2))
  println(sum1(square)(1, 2))
  println(product(square)(1, 2))

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) = {
    Math.abs((x - y) / x) / x < tolerance
  }

  def fixedPoint(f: Double => Double, firstGuess: Double) {

    def iterate(guess: Double): Double = {
      val next = f(guess)
      println("Next=" + next)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y), 1)

  println(sqrt(2))


  class Rational(x: Int, y: Int) {
    require(y > 0, "y must be positive")

    def this(x: Int) = this(x, 1)

    private def gcd(x: Int, y: Int): Int = {
      if (y == 0) x else gcd(y, x % y)
    }

    private val g = gcd(x, y)

    val numer = x / g

    val denom = y / g

    def +(other: Rational) = new Rational(numer * other.denom + other.numer * denom, denom * other.denom)

    def -(other: Rational) = new Rational(numer * other.denom - other.numer * denom, denom * other.denom)

    def *(other: Rational) = new Rational(numer * other.numer, denom * other.denom)

    def /(other: Rational) = new Rational(numer * other.denom, denom * other.numer)

    def <(other: Rational) = numer * other.denom < denom * other.numer

    def >(other: Rational) = !(<(other))

    def eq(other: Rational) = numer == other.numer && denom == other.denom

    def unary_- = new Rational(-numer, denom)


    override def toString: String = numer + "/" + denom
  }

  val x = new Rational(1, 2)
  val y = new Rational(3, 5)
  val z = new Rational(1, 2)

  println(x.numer)
  println(x.denom)
  println(x eq z)
  println(-x)


}
