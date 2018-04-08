package kai.gettingstarted

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    // 재귀함수에 대해 꼬리 호출이 실제로 제거 되었는지 확인할 때
    @annotation.tailrec
    def go(n: Int, value: Int): Int =
      if (n <= 0) value
      else go(n - 1, n * value)

    go(n, 1)
  }

  // 0 1 1 2 3 5 8 13 21 34
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, pre: Int, curr: Int): Int = {
      if (n == 0) pre
      else {
        loop(n - 1, curr, pre + curr)
      }
    }

    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-40))
    println("Factorial Value : " + factorial(5))
    println("Fibonacci Value : " + fib(8))
  }
}


object IndexSearch {
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1 // Not Found
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(findFirst(Array("12", "v", "b"), "b"))
  }
}

object PolymorphicFunctions {
  // A는 형식 변수
  def findFirst[A](ss: Array[A], f: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (f(ss(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  /**
    * partial application 부분적용
    * 값하나와 함수 하나를 입력 받고 `인수가 하나인 함수`를 결과로서 돌려준다.
    * 반환 형식이 `B => C`이라는 것은, b를 입력으로 받는 함수를 리턴한다는 것이다. (함수 리터럴)
    *
    * @param a value
    * @param f function
    * @tparam A
    * @tparam B
    * @tparam C
    * @return Function
    */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  /**
    * partial application 부분적용
    * A를 입력받은 함수에서 어떤 함수를 호출한다.
    * 어떤 함수를 파라미터 B를 받아서 C를 리턴한다.
    * ```
    * function(a) = {
    * --function(a,b) => c
    * }
    * ```
    *
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    /**
      * 배열 리터럴은 `new` 없이 생성이 가능하다. Array(1, 2, 3)
      * 함수 리터럴은 익명 함수이다. 함수의 이름을 가진 메서드로 정의
      * 해당 문법이 가능한 이유는 내부적으로 apply 함수가 구현되기 때문이다.
      * Function parameter 는 `=>`의 왼쪽
      * Function body 는 `=>`의 오른쪽
      */
    println(findFirst(Array(1, 3, 6, 8, 234, 55, 65, 13, 99), (x: Int) => x == 99))
  }
}

object Sorted {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
    // 정렬 되었다면..
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(4, 9, 20, 2), (x: Int, y: Int) => x > y))
  }
}








