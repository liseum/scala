package kai.datastructure

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/**
  * 단일 객체 선언
  * 클래스와 클래스의 유일한 인스턴스 동시에 생성
  */
object Tree {
  def size[A](root: Tree[A]): Int =
    root match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(root: Tree[Int]): Int =
    root match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth(root: Tree[Int]): Int =
    root match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }


  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // TODO
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(a) => f(a)

    }

  def main(args: Array[String]): Unit = {
    println("Tree")
    val a: Leaf[Int] = Leaf(10)
    val b: Leaf[Int] = Leaf(20)
    val c: Array[Leaf[Int]] = new Array[Leaf[Int]](10)
    for (i <- 0 to 9) {
      c(i) = Leaf(i)
    }
    var b1 = Branch(c(0), c(1))
    var b2 = Branch(c(2), c(3))
    var c1 = Branch(b1, b2)
    var b3 = Branch(c(4), c(5))
    var b4 = Branch(c(6), c(7))
    var c2 = Branch(b3, b4)
    //    var b5 = Branch(c(8), c(9))

    var root = Branch(c1, c2)
    //    println(root, b1, b2, b3, b4, b5)
    println("Size :" + size(root))
    println("Max : " + maximum(root))
    println("Depth : " + depth(root))
    println("Map : " + map(root)(x => x + 10))

  }
}
