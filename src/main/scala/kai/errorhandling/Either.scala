package kai.errorhandling


/**
  * (_ * _) => ((x, y) => x * y) 를 간결하게 표현
  * Person(_, _) = ((x, y) => Person(x,y))
  *
  */

import scala.{Option => _, Either => _, _}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      // think about it...
      // Only two cases exit.
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
  // Only two cases exit.
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      // 실패면 b를 실행
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =

  //    this.flatMap(i => b.map(j => f(i, j)))
    for {aa <- this
         bb <- b}
      yield f(aa, bb)

}

sealed class Name(val value: String)

sealed class Age(val value: Int)

case class Person(name: Name, age: Age)

/**
  * 실패
  *
  * @param value Error Value
  * @tparam E Error
  */
case class Left[+E](value: E) extends Either[E, Nothing]


/**
  * 성공. right(옳은)
  *
  * @param value Success Value
  * @tparam A Success
  */
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {

  /**
    * 평균
    *
    * @param i Seq
    * @return mean value
    */
  def mean(i: IndexedSeq[Double]): Either[String, Double] =
    if (i.isEmpty)
      Left("mean of empty list")
    else
      Right(i.sum / i.length)

  /**
    * 나누기 시 예외 처리 추가
    *
    * @param x Int
    * @param y Int
    * @return Div value or exception
    */
  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  /**
    * 에러 공통 패턴 추출 함수
    *
    * @param a
    * @tparam A
    * @return
    */
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  /**
    * List에 있는 모든 값을 Either 형태로 return
    *
    * @param es List
    * @tparam E Error Value
    * @tparam A Suscess Value
    * @return
    */
  def sequence1[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => h.flatMap(tt => sequence(es).map(tt :: _))
    }

  /**
    * List[A] -> each A to Either[E,B] -> Either[E, List[B]]
    *
    * @param as
    * @param f
    * @tparam E
    * @tparam A
    * @tparam B
    * @return
    */
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => (f(h).map2(traverse(t)(f))) (_ :: _)
    }

  def traverse1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((h, t) => f(h).map2(t)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def main(args: Array[String]): Unit = {
    println("Hello")

    println(mkPerson("Jee", 27).map(x => println(x.name.value + " " + x.age.value)))
    println(mkPerson("Sungbin", 0).map(x => println(x.name.value + " " + x.age.value)))
    println(mkPerson("", 20).map(x => println(x.name.value + " " + x.age.value)))


  }

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age <= 0) Left("Age is out of range")
    else Right(new Age(age))

  /**
    * Person 객체 만들기
    *
    * @param name
    * @param age
    * @return
    */
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))((x, y) => Person(x, y))

}




