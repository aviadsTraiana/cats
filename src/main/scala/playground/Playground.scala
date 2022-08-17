package playground

import cats.Eval
import dataManipulation.Writers.l

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object Playground {

  val meaningOfLife = Eval.later {
    println("Learning Cats: computing abstractions and the meaning of life...")
    42
  }


  trait Summable[T]{
    def sum(list:List[T]):T
  }

  //instances
  object SummableInstances {
    implicit val integers: Summable[Int] = (list: List[Int]) => list.sum
    implicit val strings : Summable[String] = (list: List[String]) => list.mkString("")
  }
  //DSL with Companion object
  object Summable{
    def apply[T](list: List[T])(implicit summable: Summable[T]) :T = summable.sum(list)
  }

  def main(args: Array[String]): Unit = {
//    import  SummableInstances._
//    println(Summable(List(1,2,3))) //6
//    println(Summable(List("a","b","c"))) //abc

    //println(meaningOfLife.value)
    def `x+1`(x:Int) =Future(x+1)
    val future = `x+1`(2)
    future.onComplete{
      case Success(x) => println(s"first ${x}")
      case Failure(exception) => println("failure 1")
    }
    future.onComplete{
      case Success(x) => println(s"second ${x}")
      case Failure(exception) => println("failure 2")
    }
    Await.result(future,Duration.Inf)
  }

}
