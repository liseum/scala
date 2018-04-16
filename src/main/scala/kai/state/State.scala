package kai.state

/**
  * @author Sungbin Yoon
  * @since 2018-04-17
  */

trait RNG {
  def nextInt: (Int, RNG)
}

/**
  * Q : case class 는 왜 하는건가 ?
  * A :
  * 1) case directive는 생서자 매개변수가 자동으로 읽기전용 필드로 바뀐다.
  * 즉, 인스터스를 생성하고 나면 매개변수를 읽기 수는 읽지만 값을 변경 할 수 없다.
  * 2) 컴파일러가 자동으로 몇 가지 메서드를 만들어 준다.
  * -> toString, equals, hashCode
  * 3) singleton 객체로 만들어진다.  companion object를 자동으로 만들어 낸다.
  *
  * > refs Programming Scala p.69
  *
  * @param seed
  */

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5de + 0xbl) & 0xffffffffffffl
    var nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

/**
  *
  * 난수 발생기는 참조투명하지 않다.
  * 상태 갱신이 부수 효과로서 수행되기 때문이다.
  * => 참조 투명성을 되찾기 위해서는 상태 갱신을 명시적을 들어내는 것이다.
  * 상태를 부수 효과로서 갱신하지 말고, 새상태를 발생한 난수와 함께 돌려주면 된다.
  **/

object State {
  def main(args: Array[String]): Unit = {
    println("State")
    val rng = SimpleRNG(52)
    println(rng)
    val (n1, rng2) = rng.nextInt
    println(n1, rng2)

  }
}


