package week5

object Week5 extends App {
  val numbers = List(7, 3, 9, 2)
  val fruits = List("apple", "orange", "kiwi")


  fruits match {
    case Nil => println("Nil")
    case (f1 :: f2 :: rest) => println(f1)
  }

  def insert(elem: Int, sortedInts: List[Int]): List[Int] = {
    sortedInts match {
      case List() => List(elem)
      case head :: tail => if (elem < head) elem :: sortedInts else head :: insert(elem, tail)
    }
  }

  def isort(list: List[Int]): List[Int] = {
    list match {
      case List() => List()
      case head :: tail => insert(head, isort(tail))
    }
  }

  println("Insertion sort=" + isort(numbers))


  def msort[T](list: List[T])(implicit lt: (T, T) => Boolean): List[T] = {
    val n: Int = list.length / 2

    if (n == 0) list
    else {

      def merge(xs: List[T], ys: List[T]): List[T] = xs match {
        case List() => ys
        case x :: xs1 =>
          ys match {
            case Nil => xs
            case y :: ys1 => if (lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
          }

      }
      val (first, second): (List[T], List[T]) = list splitAt n
      merge(msort(first)(lt), msort(second)(lt))
    }
  }

  println("Merge sort=" + msort(numbers)((x, y) => x < y))
}
