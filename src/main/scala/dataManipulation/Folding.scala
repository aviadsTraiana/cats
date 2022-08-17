package dataManipulation

import cats.{Eval, Monoid}

import scala.{+:, :+, ::}

object Folding {

  object ListEx {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((x, y) => f(x) :: y)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
      if (list.isEmpty) List.empty
      else list.foldLeft(f(list.head))((x, y) => x ++ f(y))
    }
    def filter[A](list: List[A])(pred: A => Boolean): List[A] = {
      if (list.isEmpty) List.empty
      else {
        val head = list.head
        val seed = if (pred(head)) List(head) else List.empty
        def next(x: List[A], y: A): List[A] = if (pred(y)) x :+ y else x
        list.foldLeft(seed)(next)
      }
    }
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = {
      list.foldLeft(monoid.empty)((x, y) => monoid.combine(x, y))
    }

  }

  import cats.Foldable
  import cats.instances.list._
  val sum=Foldable[List].foldLeft(List(1,2,3),0)(_ + _) //6
  import cats.instances.option._ //implicit Foldable[List]
  val sumOption= Foldable[Option].foldLeft(Option(2),30)(_ + _) //32
  import cats.Eval
  //foldRight is stack-safe regardless of the container implement foldRight
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1,2,3),Eval.now(0)){ (num, eval) =>
    eval.map(_ + num)
  }

  def catsCombineAll: Int = {
    import cats.instances.int._
    Foldable[List].combineAll(List(1,2,3)) //3 -implicit monoid[int]
  }
  import cats.instances.string._
  val mappedConcat: String = Foldable[List].foldMap(List(1,2,3))(_.toString) //implicit Monoid[String]

  //traverse nested data structures
  import cats.instances.int._
  val intNested = List(Vector(1,2,3),Vector(4,5,6))
  import cats.instances.vector._
  val combineNested: Int = (Foldable[List] compose Foldable[Vector]).combineAll(intNested) //21 = 1+2+3+4+5+6

  //ext methods:
  import cats.syntax.foldable._
  val sum3= List(1,2,3).combineAll //req Foldable[List],Monoid[Int]
  val concat2= List(1,2,3).foldMap(_.toString)

  def main(args: Array[String]): Unit = {
    import ListEx._
    val numbers = (1 to 10).toList
    import cats.instances.int._
    println(combineAll(numbers))
    println(combineNested)
  }
}
