package abstractMath

import java.util.concurrent.Executors

import apple.laf.JRSUIConstants.SegmentLeadingSeparator
import jdk.nashorn.internal.ir.BreakableNode

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object Monads extends App {
    val numberList= List(1,2,3)
    val charList=List('a','b')

    val combination1 = numberList.flatMap(n => charList.map(c=> (n,c)))
    val combination2 = for{
    n<- numberList
    c<- charList
    } yield (n,c)

    val numberOption = Option(2)
   val charOption = Option('c')

  val combination3= numberOption.flatMap(n => charOption.map(c=> (n,c)))
  implicit val ec : ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(8))
  val numberFuture = Future.successful(42)
  val charFuture = Future.successful('A')
    val combination4 = for{
        n<- numberFuture
        c<- charFuture
    } yield (n,c)

    /**
     * Pattern is:
     * -wrapping a value into M value (via pure)
     * - compose via flatmap
     */

  import cats.Monad
    import cats.instances.future._
  val fiveFuture= Monad[Future].pure(5)

  def getPairsList(numbers:List[Int],chars:List[Char]) =numbers.flatMap(n => chars.map(c=> (n,c)))
  def getPairsOption(numbers:Option[Int],chars:Option[Char]) =numbers.flatMap(n => chars.map(c=> (n,c)))
  //generalize this
  def getPairs[M[_],A,B](ma:M[A],mb:M[B])(implicit monad: Monad[M]) :M[(A,B)] = {
      monad.flatMap(ma)(a=> monad.map(mb)(b=> (a,b)))
  }
  //shorter version
    import cats.syntax.applicative._ //pure is here
    import cats.syntax.functor._ //map is here
    import cats.syntax.option._
  import cats.instances.option._
 /*   val oneOption: Option[Int] = 1.pure[Option]


  val threeOption: Option[Int] =for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one+two
  println(threeOption)*/
  import cats.syntax.flatMap._ //flatmap is here
  def getPairs2[M[_]:Monad,A,B](ma:M[A],mb:M[B]) :M[(A,B)] = ma.flatMap(a=> mb.map(b=> (a,b)))
  def getPairsFor[M[_]:Monad,A,B](ma:M[A],mb:M[B]) :M[(A,B)] =
      for{
          a<- ma
          b<- mb
      } yield (a,b)

  import cats.instances.list._
  println(getPairsFor(List(1,2),List('a','b')))

  case class Connection(host:String,port:Int)
  val config = Map(
    "host" -> "localhost",
    "port" -> "8080"
  )


  trait HttpService[M[_]]{
    def getConnection(cfg:Map[String,String]) :M[Connection]
    def issueRequest(connection: Connection,payload:String) :M[String]
  }
  object OptionHttpService extends HttpService[Option] {

      override def getConnection(cfg: Map[String, String]): Option[Connection] = for{
        h <- Option(cfg("host"))
        p <- Option(cfg("port")).map(_.toInt)
        } yield Connection(h,p)

      override def issueRequest(connection: Connection, payload: String): Option[String] = {
        if(payload.length>=20) None
        else Some(s"Request $payload has been accepted")
      }
  }

  val responseOption: Option[String] = OptionHttpService.getConnection(config) flatMap { conn =>
      OptionHttpService.issueRequest(conn, "Hello page")
  }
  val responseOptionFor :Option[String] = for {
  c<- OptionHttpService.getConnection(config)
  r<- OptionHttpService.issueRequest(c,"Hello!")
  } yield r
  println( responseOption)
  println( responseOptionFor)



  implicit object OptionMonad extends Monad[Option] {
      override def pure[A](x: A): Option[A] = Option(x)
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
      @tailrec
      override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
          case Some(Left(v)) => tailRecM(v)(f)
          case Some(Right(v)) =>Some(v)
          case None =>  None
      }
  }

  type Identity[T] = T
  val aNumber :Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity]{
      override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)

      @tailrec
      override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
          case Left(v)=> tailRecM(v)(f)
          case Right(b) => b
      }

      override def pure[A](x: A): Identity[A] = x
  }

  sealed trait Tree[+A]
  final case class Leaf[+A](value:A) extends Tree[A]
  final case class Branch[+A](left:Tree[A],right:Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree]{
      override def pure[A](x: A): Tree[A] = Leaf(x)
      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
          case Leaf(value) => f(value)
          case Branch(left,right) => Branch(flatMap(left)(f),flatMap(right)(f))
      }

      override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
        def tailRec(todo:List[Tree[Either[A,B]]],expanded:Set[Tree[Either[A,B]]],done:List[Tree[B]]): Tree[B] = {
          if(todo.isEmpty) done.head
          else todo.head match {
              case Leaf(Left(v)) => tailRec(f(v) :: todo.tail ,expanded,done )
              case Leaf(Right(b)) => tailRec(todo.tail,expanded,Leaf(b) :: done)
              case node @ Branch(left,right) =>
              if(!expanded.contains(node)){
                tailRec(right:: left :: todo, expanded+node,done)
              }else{
                val newLeft= done.head
                val newRight = done.tail.head
                val newBranch = Branch(newLeft,newRight)
                tailRec(todo.tail,expanded, newBranch::done.drop(2))
              }
          }
        }
          tailRec(List(f(a)),Set.empty,List.empty)
      }

  }

  val tree: Tree[Int] = Branch(Leaf(10),Leaf(20))
  val changedTree = TreeMonad.flatMap(tree)(x=> Branch(Leaf(x),Leaf(x+1)))
  println(changedTree)



}
