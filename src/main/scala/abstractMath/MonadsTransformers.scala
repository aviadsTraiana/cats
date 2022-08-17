package abstractMath

object MonadsTransformers {

  def sumAllOptions(values:List[Option[Int]]) : Int = ???

  import cats.data.OptionT //Option transformer , for example list of options :)
  import cats.instances.list._ // implicit OptionT[Li

  val listOfNumOptions :OptionT[List,Int] = OptionT(List(Option(1),Option(2)))
  val listOfCharOptions :OptionT[List,Char] = OptionT(List(Option('a'),Option('b'),Option.empty[Char]))
  //if we would like to normally combine the two list we would need to unwrap them first, very clunky
  //transformer can help when we have monad that wrap a monad
  val combination :OptionT[List,(Char,Int)] = for{
      number <- listOfNumOptions
      char <- listOfCharOptions
  } yield (char,number)


    def main(args: Array[String]): Unit = {
        println(combination.value) //List(Some((a,1)), Some((b,1)), None, Some((a,2)), Some((b,2)), None)
    }

    //either transformer
   import cats.data.EitherT
  val listOfEither :EitherT[List,String,Int] = EitherT(List(Left("SomethingWrong"),Right[String,Int](42),Right[String,Int](42)))



}
