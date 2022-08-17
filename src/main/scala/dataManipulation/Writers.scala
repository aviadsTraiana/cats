package dataManipulation

import scala.annotation.tailrec

//let's you track your ds while it is being manipulated
object Writers {

  type LogType = List[String]
  type ValueType = Int
  import cats.data.Writer
  //1. define at the start of the word
  val aWriter: Writer[LogType,ValueType] = Writer(List("calculating the meaning of life"),42)
  //2.manipulate as pure fp
  //modify the value
  val incWriter = aWriter.map(_+1) //change only value , no log change
  //modify the log
  val logWriterChanged = aWriter.mapWritten(_ :+ "increased the value by one") //value stays the same, log change

  //modify both
  aWriter.bimap(_ :+ "increased the value by one",_+1)// log and value change
  //modify with tuple (more powerful- can be used on both sides)
  aWriter.mapBoth { (log, value) =>
        val newValue = value+1
      (log :+ s"new value is $newValue",newValue)
  }

  //3. dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  //extract both
  val (l,v) = aWriter.run

  //writer is a monad, so it can compose
  val writerOne= Writer(Vector("logA1","logA2"),42)
  val writerTwo = Writer(Vector("logB1"),10)

  // logs are combined via the context of Semi-groups
  import cats.instances.vector._ //imports a Semigroup of vector
  val compsoseWriters = for{
    oneValue <- writerOne
    twoValue <- writerTwo
  } yield oneValue+twoValue


  import cats.instances.list._ // reset the logs (needs a Moniod[List[String]] ) to define the empty value
  aWriter.reset

    def main(args: Array[String]): Unit = {
        println(compsoseWriters.run)

       pureNaiveSum(4).written.foreach(println)
    }
  //TODO: rewrite this to be pure
  def naiveSum(n:Int) :Int ={
    if(n<=0) 0
    else{
      println(s"now sum is ${n}")
      val lowerSum = naiveSum(n-1)
      println(s"Computed naiveSum(${n-1})=$lowerSum")
      lowerSum+n
    }
  }

  def pureNaiveSum(n:Int):Writer[Vector[String],Int] = {
    @tailrec
    def recTail(n:Int,tracker:Writer[Vector[String],Int]):Writer[Vector[String],Int] ={
        if(n<=0) tracker
        else recTail(n-1, tracker
                                .mapWritten(_ :+ s"now sum is ${n}")
                                .map(v=>v+n)
                                .mapBoth{ (l,v) => (l :+ s"Computed PureNaiveSum(${n})=$v",v)
        })
    }

   recTail(n,Writer(Vector(),0))
  }



}
