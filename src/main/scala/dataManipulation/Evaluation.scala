package dataManipulation

object Evaluation {
    import cats.Eval

  val eagerEvaluation: Eval[Int] = Eval.now{ //will be evaluated eagerly
    println("Computing right now")
    42
  }

  val evalJustLazily :Eval[Int] = Eval.always{
      println("Computing only when calling .value without memoization")
      42
  }

  val evalLazyMemoization :Eval[Int] = Eval.later{
    println("Computing only on first call . value is saved")
    42
  }

    def main(args: Array[String]): Unit = {
        //eagerEvaluation already printed by now
        //get the value
        println(eagerEvaluation.value)
        println("-------------------------------------------")
        //lazy, no memoization
        println(evalJustLazily.value) //will print and then will print 42
        println(evalJustLazily.value) //will print and then will print 42

        println("-------------------------------------------")
        println(evalLazyMemoization.value) //will print and then will print 42
        println(evalLazyMemoization.value) //will print 42 since the result was saved

        println("-------------------------------------------")
        //Eval is a monad, so it can compose
        println(eagerEvaluation.flatMap(value1=> evalLazyMemoization.map(value2=> value1+value2)).value) //prints 84
        //and of course can use for-comprehension
        val eFor= for{
                    value1 <- eagerEvaluation
                            value2 <- evalLazyMemoization
        } yield value1+value2
        println(eFor.value)
        println("-------------------------------------------")

        val evalEX1= for{
                    a<- evalLazyMemoization
                            b<- evalJustLazily
                            c<- eagerEvaluation
                            d<- evalJustLazily
        } yield a+b+c+d

        println(evalEX1.value) //prints: right now(at the beginning of the program) , only when calling , only when calling , sum
        println(evalEX1.value) //prints: only when calling, only when calling , sum (eager was already saved in the val)

        println("-------------------------------------------")
        //we can also change from just lazy evaluation to lazy evaluation+memoization
        val dontRecompute= evalJustLazily.memoize
        println(dontRecompute.value) //prints and then 42
        println(dontRecompute.value) //42
        println("-------------------------------------------")

        println("TODO: implement defer such that defer(Eval.now) does not run the side effects")
        def defer[T](eval: => Eval[T]) : Eval[T] = Eval.later(()).flatMap(_=> eval) //is it identical to Eval.later(eval.value) ? yes
        println(defer(Eval.now{
          println("now!")
          42
        }).value)

      //defer can help us avoid stack overflow recursion!
      // for example
      def reverseListNaive[T](list:List[T]):List[T] = if( list.isEmpty) list  else reverseListNaive(list.tail) :+ list.head

      def reverseListEval[T](list:List[T]):Eval[List[T]] ={
        if(list.isEmpty) Eval.now(list)
        else reverseListEval(list.tail).map(_ :+ list.head)
      }
      def reverseListDeferEval[T](list:List[T]):Eval[List[T]] ={
            if(list.isEmpty) Eval.now(list)
            else Eval.defer{ //like our defer implementation supplied by cats
                reverseListDeferEval(list.tail).map(_ :+ list.head)
            }
      }

      //reverseListNaive((1 to 10_000).toList) // will stack overflow
      //reverseListEval((1 to 10_000).toList) // will also stack overflow
      println(reverseListDeferEval((1 to 10_000).toList).value) // Stack safe :)

    }
}
