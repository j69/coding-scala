import scala.concurrent._
import scala.concurrent.duration._

implicit val ec: ExecutionContext = ExecutionContext  .fromExecutor(java.util.concurrent.Executors.newFixedThreadPool(5))

// batch processing (grouping) futures

def calc(i: Int): Future[Int] = Future {
  println(s"${System.currentTimeMillis()}Calculating for $i")
  Thread.sleep(5000)
  i * i
}

def processSeq(xs: Vector[Int]): Future[Vector[Int]] = {
  val allFutures: Vector[Future[Int]] = xs.map(calc)
  Future.sequence(allFutures)
}

def processSeq2(xs: Vector[Int]): Future[Vector[Int]] = {
  Future.traverse(xs)(calc)
}

def processSeqBatch(xs: Vector[Int], batchSize: Int): Future[Vector[Int]] = {
  val batches = xs.grouped(batchSize)
  val start = Future.successful(Vector.empty[Int])
  batches.foldLeft(start) {(accF, batch) =>
    for {
      acc <- accF
      batchRes <- processSeq(batch)

    } yield {
      println (System.currentTimeMillis() +    " calculated "  + batchRes)
      acc ++ batchRes
    }
  }
}

val nums = (1 to 20).toVector

/*
val f = processSeq(nums)
Await.result(f, 20.seconds)
*/

val f2 = processSeqBatch(nums, 2)
Await.result(f2, 20.seconds)
