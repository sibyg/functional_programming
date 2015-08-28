package week4

object Week4 extends App {

  trait List[+T] {
    def isEmpty: Boolean

    def head: T

    def tail: List[T]

    def get(index: Int): T
  }

  class Nil[T] extends List[T] {
    override def isEmpty: Boolean = true

    override def tail: List[T] = throw new NoSuchElementException("Nil.tail")

    override def head: T = throw new NoSuchElementException("Nil.head")

    override def get(index: Int): T = throw new Error("Nil.get")

    override def toString: String = ""
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    override def isEmpty: Boolean = false

    override def get(index: Int): T = {

      def getF(list: List[T], currentIndex: Int): T = {
        if (list.isEmpty) throw new IndexOutOfBoundsException
        else if (currentIndex == index) list.head
        else getF(list.tail, currentIndex + 1)
      }

      getF(this, 0)
    }

    override def toString: String = head + ", " + tail
  }

  def singleton[T](element: T) = new Cons[T](element, new Nil[T])

  val list: List[Int] = new Cons[Int](3, new Cons[Int](4, new Nil[Int]))

  println(list)
  println(list get 0)
  println(list get 1)

  //  println(list get 2) // will throw IndexOutOfBoundsException

  abstract class Booleann {
    def ifThenElse[T](t: T, e: T): T

    def &&(x: Booleann) = ifThenElse(x, false)

    def ||(x: Booleann) = ifThenElse(true, x)

    def unary_- = ifThenElse(false, true)

    def ==(x: Booleann) = ifThenElse(x, -x)

    def !=(x: Booleann) = ifThenElse(-x, x)
  }

  object True extends Booleann {
    override def ifThenElse[T](t: T, e: T): T = t
  }

  object False extends Booleann {
    override def ifThenElse[T](t: T, e: T): T = e
  }

  // Piano numbers
  abstract class Nat {
    def isZero: Boolean

    def predecessor: Nat

    def successor: Nat = new Succ(this)

    def +(that: Nat): Nat

    def -(that: Nat): Nat
  }


  object Zero extends Nat {
    override def isZero: Boolean = true

    override def predecessor: Nat = throw new NoSuchElementException("0.predecessor")

    override def +(that: Nat): Nat = that

    override def -(that: Nat): Nat =
      if (that.isZero) Zero else throw new NoSuchElementException("0.-")
  }

  class Succ(n: Nat) extends Nat {
    override def isZero: Boolean = false

    override def +(that: Nat): Nat = new Succ(n + that) //TODO DOUBTFUL

    override def -(that: Nat): Nat = if (that.isZero) n else n - that.predecessor

    override def predecessor: Nat = n
  }

  println(new Succ(Zero))

  trait Expr {
    def isNumber: Boolean

    def isSum: Boolean

    def numValue: Int

    def leftOp: Expr

    def rightOp: Expr

    def eval(e: Expr): Int = {
      if (e.isNumber) e.numValue
      else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
      else throw new Error("Unknown expression:" + e)
    }

    def show(e: Expr): String = ???
  }

  case class Num(n: Int) extends Expr {
    override def isNumber: Boolean = true

    override def rightOp: Expr = throw new Error("Num.rightOp")

    override def numValue: Int = n

    override def isSum: Boolean = false

    override def leftOp: Expr = throw new Error("Num.leftOp")
  }

  case class Sum(e1: Expr, e2: Expr) extends Expr {
    override def isNumber: Boolean = false

    override def rightOp: Expr = e2

    override def numValue: Int = throw new Error("Sum.numValue")

    override def isSum: Boolean = true

    override def leftOp: Expr = e1
  }

  case class Prod(e1: Expr, e2: Expr) extends Expr {
    override def isNumber: Boolean = ???

    override def rightOp: Expr = ???

    override def numValue: Int = ???

    override def isSum: Boolean = ???

    override def leftOp: Expr = ???
  }

  case class Var(n: Number) extends Expr {
    override def isNumber: Boolean = ???

    override def rightOp: Expr = ???

    override def numValue: Int = ???

    override def isSum: Boolean = ???

    override def leftOp: Expr = ???
  }



}
