package abstractMath

object Applicatives {
 //Applicatives = functor+ pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val list: List[Int] = listApplicative.pure(2)

  import cats.instances.option._ //implicit Applicative[Option]
  val optionNumber: Option[Int] = Applicative[Option].pure(2)

  //extention methods
  import cats.syntax.applicative._
  val aSweetList: List[Int] = 2.pure[List]
  val aSweetOption: Option[Int] = 2.pure[Option]

  //monads extends applicatives
  //applicatives extends functors

  //validated for example does not follow the monads law, but he is following the applicative laws
  import cats.data.Validated
  type ErrorOr[T] = Validated[List[String],T]
  val validValue:ErrorOr[Int] = Validated.valid(43) // "pure"

  //todo: thought exercise
  //def ap[W[_],B,T](wf:W[B=>T])(wa:W[B]):W[T] = ???
  def productWithApplicatives[W[_],A,B](wa:W[A],wb:W[B])(implicit applicative: Applicative[W]) : W[(A,B)] ={
    val wf: W[B => (A, B)] = applicative.map(wa)(a=> (b:B) => (a,b))
    applicative.ap(wf)(wb)
  }

  //Applicatives have this ap
  //Applicatives can implement product from Semigroupal => Applicatives extends Semigroupal


    def main(args: Array[String]): Unit = {

    }
}
