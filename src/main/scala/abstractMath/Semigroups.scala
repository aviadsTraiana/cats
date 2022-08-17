package abstractMath

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Random


object Semigroups {

    //Semigroup combine elements of the same type (that are associative)
    import cats.Semigroup
    import cats.instances.int._
    import cats.instances.map._
    import cats.instances.option._
    val intSemiGroup: Semigroup[Int] = Semigroup[Int]
    val intCombination: Int = intSemiGroup.combine(2,46)
    import cats.instances.string._
    val stringCombination: String =Semigroup[String].combine("I love ","cats")

    //create semigroup for custom class
    case class Expense(id:Long,amount:Int,price:Long)
    implicit object ExpenseSemiGroup extends Semigroup[Expense]{
        override def combine(x: Expense, y: Expense): Expense = Expense(hash(x.id,y.id),calcAmount(x,y),calcPrice(x,y))

        private def hash(id1:Long,id2:Long) = (id1+id2)/2 % new Random().nextLong(100) //some logic of hashing - really bad one
        private def calcAmount(x:Expense,y:Expense) = x.amount+y.amount
        private def calcPrice(x:Expense,y:Expense) =  (popularity(x.id)*x.price)+(popularity(y.id)*y.price)
        private def popularity(id:Long) : Long = new Random().nextLong(id)
    }

    // Extension methods, combine with |+| operator
    import cats.syntax.semigroup._
    val combineExpanses: Expense = Expense(1,1,100)  |+| Expense(2,10,10)


    private def reduceInts(list:List[Int]) = list.reduce(intSemiGroup.combine)

    private def reduceT[T](list:List[T])(implicit semigroup: Semigroup[T]) : T = list.reduce(semigroup.combine)

    private def reduceT2[T:Semigroup](list:List[T]) : T = list.reduce(_ |+| _)


    def main(args: Array[String]): Unit = {
        println(intCombination)
        println(stringCombination)
        val numbers= List(1,2,3)
        println(reduceInts(numbers))
        println(reduceT(List("Hi"," Man!")))
        val optList= numbers.map(x=> Option(x))
        import cats.instances.option._
        println(reduceT(optList)) // produce sum of elements inside of option, none if one of them is none

        //custom semi group :)
        val expenseList= List(Expense(1,2,100),Expense(2,5,10))
        println(reduceT(expenseList))
        println(reduceT2(expenseList))
        println(reduceT2(List("ONE")))
        val optionMap = Option(Map(("x"->"2"))) |+| Option(Map("y" -> "1"))
        println(optionMap)
        println( Option(Map(("x"->"2"))) |+| None)




    }
}
