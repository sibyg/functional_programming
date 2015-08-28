package week3

object Week3 extends App {

  abstract class IntSet {
    def incl(elem: Int): IntSet

    def contains(elem: Int): Boolean

    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    override def incl(elem: Int): IntSet = new NonEmpty(elem, Empty, Empty)

    override def contains(elem: Int): Boolean = false

    override def union(other: IntSet): IntSet = other

    override def toString: String = "_"
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    override def contains(x: Int): Boolean =
      if (x < elem) left.contains(elem)
      else if (x > elem) right.contains(elem)
      else true

    override def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    //TODO understand it...union is calling itself...after every iteration number of sets is decrementing
    override def union(other: IntSet): IntSet = left union right union other incl elem

    override def toString: String = left + " " + elem + " " + right
  }

  println(new NonEmpty(5, Empty, new NonEmpty(6, Empty, Empty)) union new NonEmpty(3, Empty, Empty))


}
