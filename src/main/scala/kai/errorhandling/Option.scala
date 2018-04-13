package kai.errorhandling

/**
  * 고차 함수(HOF, high-order function)
  * 다른 함수를 인수로 받는 함수
  *
  * 완전 함수(total function)
  * 함수가 입력을 처리할 수 없는 상황에 처했을 때 무엇을 해야하는지 말해주는 인수를 호출자가 지정
  *
  * trait(특질)
  * 추상 인터페이스 선언
  *
  * sealed
  * 해당 파일에서 모든 구현이 이루어져야 한다.
  **/


/**
  * 지금까지 정의해왔던 방식의 에러 접근법은 ...
  * 에러가 발생하는 경우 가짜 값을 전달 or 원하는 값 대신 Null 값을 전달
  *
  * # 한계
  *  - 오류가 소리 없이 전파. 개발자가 Null 예외처리를 안하면 무용지물
  *  - 모든 Function에 Null 처리를 직접적으로 해줘야
  *  - [A] 다형적 코드에 적용하기 힘들다. Null은 기본 형식이 아닌 형식에만 유효.
  *    - But, Int Double는 기본 형식이다.
  *  - 모든 인수를 균일하게 처리 해야 하는 고차 함수를 전달하기가 힘들어 진다.(기본형 or Other)
  *
  * # 대안
  *  - Direct Function(직접 호출자)과 같이 예외가 발생하게 되면 실행 해야 하는 함수를 추가한다면?
  *   - 이는 완전 함수(total function)을 의미.
  *   - 항상 하나의 값은 리턴되어야 한다.
  *   - 정의되지 않는 함수에 대한 직접 호출자를 알 수 가 없다.
  *
  * # 해법
  *  - 함수가 항상 답을 내지는 못한다는 점을 반환 형식을 통해서 명시적으로 표현
  **/


//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  /**
    *
    * B >: A , B가 반드시 A와 같거나 A의 상위형식 이어야 함
    * (default: => B) is non-strictness(비엄격성)
    *
    * @param default
    * @tparam B
    * @return
    */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * flatMap을 이용
    * 순차열의 편균이 m
    * 순차열의 각 요소 x에 대한 math.pow(x-m)들의 평균
    */
  def variance(xs: Seq[Double]): Option[Double] =

  /**
    *
    * xs의 평균 m
    * 순차열 각 x에 대한 ** 평균
    * **는 `math.pow(x - m, 2)`
    */
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /**
    * 2 개의 이항함수를 거쳐(A,B) 결합하여 하나의 함수를 만드는(C)를 함수를 구현
    * b.flatMap이 아닌 f.map을 사용한 이유는 함수 호출자 형식이 Option[B] 이냐 B 이냐의 차이
    * f 함수는 Option 형식이 아닌 기본 자료형이기 때문에...
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(i => b.map(j => f(i, j)))


  /**
    *
    *1. Option 목록을 받고
    *2. 모든 Some 값으로 구성된 목록을 담은 Option을 돌려주는 함수
    * None => None
    * Other => Some(Other)
    *
    * @param a
    * @tparam A
    * @return
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(tt => sequence(t).map(tt :: _))
    }

  /**
    * List[String] ->each string to Option(Int) -> Option(List[Int])
    *
    * @param a
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))


  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def main(args: Array[String]): Unit = {
    println(mean(Seq(1, 2, 3, 4, 5)))
  }

}
