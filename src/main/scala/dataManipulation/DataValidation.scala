package dataManipulation

import cats.Semigroup
import dataManipulation.DataValidation.FormValidation.FormValidation

import scala.util.Try

object DataValidation {

    import cats.data.Validated //like the either monad
  val aValidValue :Validated[String,Int] = Validated.valid(42) // "right" value
  val anIValidValue :Validated[String,Int] = Validated.invalid("something went wrong") // "left" value
  val aTest :Validated[String,Int] = Validated.cond(42>39,99,"meaning of life is too small")

  //if that like Either why do we need this? different contract used to combine all values with no mutation
  // meaning, it has nicer api

  //example: we would like to test things on number, and get all the conditions it does not fulfil
  def testNumber(n:Int): Either[List[String],Int] = {
    val isEven: List[String] = if(n%2==0) List() else List("Number must be even")
    val isNegative :List[String] = if(n>=0) List() else List("Number must be non-negative")
    val isTooBig :List[String] = if(n<=100) List() else List("Number is too big")
    if  (n%2==0 && n>=0 && n<=100) Right(n)
    else Left(isNegative ++ isEven ++ isTooBig)
  }

  //not very elegant... but with validated we can take advantage of combine


    def validateNumber(n:Int): Validated[List[String],Int] ={
      import cats.instances.list._
      implicit val combineIntMax: Semigroup[Int] = Semigroup.instance(Math.max)
      Validated.cond(n%2==0,n,List("Number must be even"))
              .combine(Validated.cond(n>=0,n,List("Number must be non-negative")))
              .combine(Validated.cond(n<=100,n,List("Number is too Big")))
    }

    //chain
    aValidValue.andThen(_ => anIValidValue) //unlike flatmap does not short-circuit
    //test a valid value
    aValidValue.ensure(List("something went wrong"))(_%2==0)
    //transform
    aValidValue.map(_+1)
    aValidValue.leftMap(_.length)
    aValidValue.bimap(_.length,_ +1)
    //conversion from stdlib
    val eitherToValidated : Validated[List[String],Int] = Validated.fromEither(Right(42))
    val optionToValidated : Validated[List[String],Int] = Validated.fromOption(Option(42),List("Represent nothing value"))
    val tryToValidated : Validated[Throwable,Int] = Validated.fromTry(Try("Something".toInt))
    //backward
    eitherToValidated.toEither
    optionToValidated.toOption

  /**
   * Exercise
   *
   * </br>-------------</br></br>
   * the forms map name,email password
   * need to validate that
   * - name ,email and password are not missing
   * - they are not blank
   * - email have "@"
   * - password is at least 10 chars
   *
   */
    object FormValidation{
    type FormValidation[T] = Validated[List[String],T]

      import cats.instances.list._
      import cats.instances.string._

      private def getValue(form:Map[String,String],field:String) : FormValidation[String] =
        Validated.fromOption(form.get(field),List(s"$field is missing in form"))
      private def nonBlank(value:String, field:String) : FormValidation[String]=
        Validated.cond(value.trim.length>0,value,List(s"$field must not be blank"))
      private def validateEmail(email:String): FormValidation[String] =
       Validated.cond(email.contains('@'),email,List("email is not valid, must contain @"))
      private def validatePassword(password:String): FormValidation[String]=
       Validated.cond(password.length>=10,password,List("password is not valid. must be at least 10 chars"))

      def validateForm(form : Map[String,String]) : FormValidation[String] = {

        getValue(form,"name").andThen(name=> nonBlank(name,"name"))
                .combine(getValue(form,"password").andThen(password=>nonBlank(password,"password")).andThen(validatePassword))
                .combine(getValue(form,"email").andThen(email=> nonBlank(email,"email")).andThen(validateEmail))
                .map(_ => "Success :)")


      }

    }

  import cats.syntax.validated._
  val meaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val invalidValue: Validated[List[String], Int] = List("error").invalid[Int]
    def main(args: Array[String]): Unit = {

      val validated: FormValidation[String] = FormValidation.validateForm(Map(
        "name" -> "",
        "password" -> "",
        "email" -> ""
      ))

      println(validated)
    }
}
