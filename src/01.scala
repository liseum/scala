object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, value: Int): Int =
      if (n <= 0) value
      else go(n - 1, n * value)

    go(n, 1)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-40))
    println("Factorial Value : " + factorial(5))
  }

}