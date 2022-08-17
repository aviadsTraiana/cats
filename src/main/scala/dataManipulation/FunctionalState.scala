package dataManipulation

import cats.data.IndexedStateT
import cats.Eval

object FunctionalState {
    type MyState[S,A] = S => (S,A)
    //useful for write and read
    import cats.data.State
    val countAndSay : State[Int,String] = State(currentCount => (currentCount+1,s"Counted $currentCount"))

    val (eleven,counted10) = countAndSay.run(10).value //returns Eval that why we call value

    //why we need this?
    // now we can 'iterate' by composing in a pure FP way :)
    val firstTransformation: State[Int, String] = State((s:Int) => (s+1,s"Added 1 to s - obtained ${s+1}"))
    val secondTransformation: State[Int, String] = State((s:Int) => (s*5,s"Mul 5 to s - obtained ${s*5}"))
    val composition: State[Int, (String, String)] =
        firstTransformation.flatMap(firstResult => secondTransformation.map(secondResult=>(firstResult,secondResult)))

  val forComposition: State[Int,(String,String)] = for{
    first <- firstTransformation
   second <- secondTransformation
  }yield (first,second)

  //function composition can get clunky
  val func1: Int => (Int, String) = (s:Int) => (s+1,s"Added 1 to s - obtained ${s+1}")
  val func2: Int => (Int, String) = (s:Int) => (s*5,s"Mul 5 to s - obtained ${s*5}")

  val compositeFunc: Int => (String, (Int, String)) = func1.andThen{
      case (newState,firstResult) => (firstResult,func2(newState))
  } //the updated state is nested inside! and decompose as we go on!

    //todo: Online store
    case class ShoppingCart(items:List[String],total:Double)
    def addToCart(item:String,itemPrice:Double) : State[ShoppingCart,Double] =  {
        def transformPrice = (startPrice:Double) => startPrice+itemPrice
        def transformCart = (s:ShoppingCart) =>  ShoppingCart(item :: s.items,transformPrice(s.total))
        State(  shoppingCart => (transformCart(shoppingCart) ,transformPrice(shoppingCart.total))   )
    }

      //TODO PURE GYMNASTICS
        //returns a state data structure that, when run, will not change the state but will issue the value f(a)
      def inspect[A,B](f:A=>B) : State[A,B] = State((a:A) => (a,f(a)))
      //returns a state data structure , when run, returns the value of that state and makes no changes
      def get[A]:State[A,A] = State(s=> (s,s))
      //returns a state data structure , when run, returns the Unit and sets the state to that value
      def set[A](value:A) : State[A,Unit] = State( _ => (value,()))

      //returns a state data structure , when run, returns the Unit(does not effect the answer) and sets the state to that f(state)
      def modify[A](f:A=>A ) : State[A,Unit] = State( s => (f(s),()))

    // all of these helper methods exist in the companion object of State
    import cats.data.State._
    //with this we can write Imperative programs in pure FP way

    val program :State[Int,(Int,Int,Int)] = for {
        a <- get[Int]
        _ <- set[Int](a+5)
        b <- get[Int]
        _ <- modify[Int]( _ * 5)
        c <- inspect[Int,Int](_*2)
    } yield (a,b,c)

    def main(args: Array[String]): Unit = {
        println(eleven) //11
        println(counted10) //prints Counted 10
      println(composition.run(10).value) //(55,(Added 1 to s - obtained 11,Mul 5 to s - obtained 55))
      println(forComposition.run(10).value) //(55,(Added 1 to s - obtained 11,Mul 5 to s - obtained 55))
      println(compositeFunc(10))//(Added 1 to s - obtained 11,(55,Mul 5 to s - obtained 55))

      //this wil mutate the shopping cart in pure way, we need the total state only at the end
      val aviadCart :State[ShoppingCart,Double] = for {
      _ <- addToCart("Shampoo",22)
      _ <- addToCart("ToothBrash",17)
      totalItemPrice <- addToCart("Lazer",10_000)
      } yield totalItemPrice
      val (cart,total) =aviadCart.run(ShoppingCart(List(),0)).value
      println(s"list of items: ${cart.items}. with total price of $total")//list of items: List(Lazer, ToothBrash, Shampoo). with total price of 10039.0

      println(program.run(0).value) //(25,(0,5,50))

    }
}
