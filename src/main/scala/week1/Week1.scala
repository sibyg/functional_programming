package week1

object Week1 extends App {
  def square(x: Double) = x * x

  def sumOfSquare(x: Double, y: Double) = square(x) + square(y)

  def loop: Boolean = loop

  def and(x: Boolean, y: => Boolean): Boolean = if (x) y else false

  def or(x: Boolean, y: => Boolean): Boolean = if (x) x else y


  def sqrt(x: Double): Double = {

    def isGoodEnough(guess: Double) = Math.abs(square(guess) - x) < 0.001
    def improve(guess: Double) = (guess + x / guess) / 2

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    sqrtIter(1.0)
  }

  println(sqrt(4))

  val trueAndTrue = and(true, true)
  val trueAndFalse = and(true, false)
  val falseAndFalse = and(false, false)
  val falseAndTrue = and(false, true)

  val falseAndLoop = and(false, loop)
  println("trueAndTrue=" + trueAndTrue)
  println("trueAndFalse=" + trueAndFalse)
  println("falseAndFalse=" + falseAndFalse)
  println("falseAndTrue=" + falseAndTrue)
  println("falseAndLoop=" + falseAndLoop)


  val falseOrFalse = or(false, false)
  val falseOrTrue = or(false, true)
  val trueOrFalse = or(true, false)
  val trueOrTrue = or(true, true)
  val trueOrLoop = or(true, loop)

  printf("%nfalseOrFalse=%b,falseOrTrue=%b,trueOrFalse=%b,trueOrTrue=%b,trueOrLoop=%b",
    falseOrFalse, falseOrTrue, trueOrFalse, trueOrTrue, trueOrLoop)

}
