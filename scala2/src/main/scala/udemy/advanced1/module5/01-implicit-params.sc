case class RetryParams(times: Int)
import scala.util.control.NonFatal
def retryCall[A](fn: => A, currentTry: Int = 0)
    (retryParams: RetryParams): A = {
  try fn
  catch {
    case NonFatal(ex) if currentTry < retryParams.times =>
      retryCall(fn, currentTry + 1)(retryParams)
  }
}
def retry[A](fn: => A)( implicit retryParams: RetryParams): A =
  retryCall(fn, 0)(retryParams)


var x = 0
def checkIt(): Int = {
  x = x + 1
  require(x > 4, "x not big enough")
  x
}

implicit val retries = RetryParams(5)


retry {
  println(s"Checking $x")
  checkIt()
}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

Future(1)