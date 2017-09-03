import scala.annotation.tailrec

object Fib {

  def fib(n: Int): Int = {
    @tailrec
    def fib(n: Int, prev: Int, next: Int): Int = {
      if (n == 0) prev
      else {
        fib(n - 1, next, prev + next)
      }
    }

    fib(n, 0, 1)
  }

}
