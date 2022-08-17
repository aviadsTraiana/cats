package dataManipulation

import java.util.concurrent.Executors

import cats.Monad

import scala.concurrent.ExecutionContext
import scala.util.Try

object ErrorHandling {
    trait MyMonadError[M[_],E] extends Monad[M]{
      def raisedError[A](e:E)
    }

   import cats.MonadError
   import cats.instances.either._ //implicit MonadError
   type ErrorOr[A] = Either[String,A]
   val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr,String]

    val success: ErrorOr[Int] = monadErrorEither.pure(32) // Either[String,Int] == Right(32)
   val failure: ErrorOr[Int] = monadErrorEither.raiseError[Int]("something wrong")

  // like recover in Try
   val handeFailure: Either[String,Int] = monadErrorEither.handleError(failure){
       case "Badness" => 44
       case _ => 89
   }
    //like rocverWith in Try
   val handleError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure){
       case "Badness" => monadErrorEither.pure(44) //ErrorOr[Int]
       case _ => Left("Something") //ErrorOr[Int]
   }
  // "filter"
  val filteredSucess: ErrorOr[Int] = monadErrorEither.ensure(success)("number too small")(_ > 100)
  //Try and Future
  import cats.instances.try_._ // implicit MonadError[Try] , E = Throwable
  val exception = new RuntimeException("really bad")
  val pureException: Try[Int] = MonadError[Try,Throwable].raiseError(exception) // store the exception

    val executionContext: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    import cats.instances.future._

  //MonadError[Future,Throwable].raiseError(exception)

  //for validated, since it is applicative there is ApplicativeError
    import cats.data.Validated
    import cats.instances.list._
    import cats.ApplicativeError
  type ErrorsOr[T]= Validated[List[String],T]
  val applErrorVal: ApplicativeError[ErrorsOr, List[String]] = ApplicativeError[ErrorsOr,List[String]]
  // have pure , raiseError,handleError,handleErrorWith (without ensure)



  //extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError,handleError,handleErrorWith
  val extendedSucess: ErrorsOr[Int] = 42.pure[ErrorsOr] //requires the implicit ApplicatioveError[ErrorsOr,List[String]
 val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr,Int]

  val recoveredError: ErrorsOr[Int] = extendedError.recover {
      case _ => 43
  }

  import cats.syntax.monadError._ //ensure extension
  val testedSuccess: ErrorOr[Int] = success.ensure("too small")(_ > 100)
    def main(args: Array[String]): Unit = {


    }
}
