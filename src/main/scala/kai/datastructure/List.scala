package kai.datastructure

/**
  * 함수적 자료구조란, 오직 순수 함수만으로 조작되는 자료구조이다.
  * 순수 함수는 자료를 그 자리에서 변경하거나 기타 부수 효과를 수행하는 일이 없어야 한다.
  * 따라서 함수적 자료구조는 정의에 의해 `불편`이다.
  * ex) 3 + 4 = 7, 기존의 메모리 복사가 없이 새로운 정수 7이 생성된다.
  * Q) 메모리 복사가 많이 일어나지 않을까??
  */


/**
  * Trait(특질). 즉, 추상인터페이스
  * sealed : 모든 구현이 반드시 이 파일 안에서 서언되어야 한다.
  * `+` : 공변(covariant) : 하위 형식이라는 조건을 만족하는 형식을 구축하기 위해 사용?...
  *
  * @tparam A
  */
sealed trait List[+A]


/**
  * 2가기 자료 생성자
  * Nil 표기법을 통해서 빈 List를 구축할 수 있게 되었다.
  */
case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
  * List 생성과 조작 메서드를 담는 그릇
  * 단일 객체 선언
  * 클래스와 클래스의 유일한 인스턴스 동시에 생성
  */
object List {
  /**
    * 패턴 부합
    */
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0

    /**
      * 임의의 표현식과 부합하는 변수 패턴`_`을 사용
      */
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }


  /**
    * 목록에 대한 재귀와 고차함수로의 일반화
    * sum, product method의 경우에 중복코드를 발견할 수 있다.
    */

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => {
        println("Nil : " + z)
        z
      }
      case Cons(x, xs) => {
        println(x + "-" + xs + "-" + z)
        f(x, foldRight(xs, z)(f))
      }
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, len) => len + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => {
        println("Nil : " + z)
        z
      }
      case Cons(h, t) => {
        println(h + "-" + t + "-" + z)
        foldLeft(t, f(z, h))(f)
      }
    }

  // ex 3.12
  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  //  foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](ns: List[A]): Int =
    foldLeft(ns, 0)((len, h) => len + 1)


  /**
    * reverse List(1, 2, 3) => List(3, 2 , 1)
    */

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((t, h) => Cons(h, t))

  /**
    * 스칼라 가변 인수 함수
    * 가변 인수 함수 구현을 통해 함수 인수중 리터럴 구문으로 호출할 수 있다.
    *
    * @param as
    * @tparam A
    * @return
    */
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * 자료 공유
    * case 1
    * Cons(xs) 앞에 1을 추가하고 싶은 경우 Cons(1, xs)를 호출 하면 된다.
    * xs는 실제로 복사할 필요가 없고 재사용된다.
    * 자료를 수정에 대한 걱정 없이, 불변이 자료구조를 return 하면 된다.
    *
    * case 2
    * Cons(1,xs) 에서 1을 제거하고 싶으면 목록 xs만 return 하면 된다.
    * 기본의 Cons(1, xs)는 유지한채 ... 이를 두고 영속적(persistent)라 한다.
    *
    * 이를 통해 자료구조에 연산이 가해져도 기존의 참조들이 결코 변하지 않는다.
    *
    */
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  /**
    * f함수에 의해서 요소 제거
    * 호출 법
    * var ex = dropWhile(List(1,2,3,4,5), (x: Int) => x < 4)
    * 익명 함수 x에 대한 자료형을 명시 해주어야한다 ...
    *
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  /**
    * scala가 익명함수의 자료형을 추론할 수 있다.
    * dropWhile(l)(f) : 즉 dropWhile은 curr 되었다.
    * like f(g)
    *
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }


  /**
    * 하나의 목록을 다른 목록의 끝에 추가
    *
    * @param a1
    * @param a2
    * @tparam A
    * @return
    */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => {
        Cons(h, append(t, a2))
      }
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1, a2)((h, t) => Cons(t, h))

  //  foldRight(a1, a2)(Cons(_ , _))

  /**
    * 마지막 tail 제거
    *
    * @param l
    * @tparam A
    * @return
    */

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    println("ex1 : " + ex1)

    val ex2: List[Int] = Cons(1, Nil)
    println("ex2 : " + ex2)

    val ex3: List[String] = Cons("ass", Cons("bb", Nil))
    println(ex3)

    val x = List(1, 2, 3, 4, 5) match {
      // fail
      case Cons(x, Cons(2, Cons(4, _))) => x
      // success
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      // success
      case Cons(h, t) => h + sum(t)
      // fail
      case Nil => 43
      // success
      case _ => 100
    }
    println(x)

    println("==tail==")
    //    tail(ex1)
    println(tail(ex2))
    println(tail(ex3))
    println(tail(List(1, 2, 3, 4, 5)))

    println("==setHead==")
    //    println(setHead(ex1, "hoho"))
    println(setHead(ex2, "hoho"))

    println("==drop==")
    println(drop(List(1, 2, 3, 4, 5), 2))

    println("==sum==")
    println(sum2(List(1, 2, 3, 4)))
    println(sum3(List(1, 2, 3, 4)))

    println("==append==")
    println(append(List(1, 2, 3, 4), List("a", "b", "c")))
    println(append2(List(1, 2, 3, 4), List("a", "b", "c")))

    println("==init==")
    println(init(List(1, 2, 3, 4, 5)))

    println("===prodiction")
    println(product2(List(1, 2, 3, 4)))
    println(product3(List(1, 2, 3, 4)))

    println("==reverse==")
    println(reverse(List(1, 2, 3, 4)))


  }
}


// ex 3.8
/**
  * foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
  * foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
  * Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
  * Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
  * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
  * Cons(1, Cons(2, Cons(3, Nil)))
  */
