package abstractMath

import scala.util.Try

object Functors extends App {

  val incList= List(1,2,3).map(_+1)
  val incTry = Try(42).map(_+1)

  //simplified definition
  trait MyFunctor[F[_]]{
    def map[A,B](initValue:F[A])(f:A=>B) :F[B]
  }

  import cats.Functor
  import cats.instances.list._
  val incList2: List[Int] = Functor[List].map(List(1,2,3))(_+1) //List(2,3,4)
  import cats.instances.option._
  val incOpt: Option[Int] = Functor[Option].map(Option(1))(_+1) //Some(2)
  import cats.instances.try_._
  val incTry2: Try[Int] = Functor[Try].map(Try(42))(_+1) //Success(43)


  //todo: define your own functor for a binary tree
  trait Tree[+T]
  object Tree{
    def leaf[T](value:T) :Tree[T] = Leaf(value)
    def branch[T](value:T,left:Tree[T],right:Tree[T]) : Tree[T]= Branch(value,left, right)
  }
  final case class Leaf[+T](value:T) extends Tree[T]
  final case class Branch[+T](value:T,left:Tree[T],right:Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(v,l,r) =>  Branch(f(v),map(l)(f),map(r)(f)) //not stack safe
      }
    }
  }


  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]):F[Int] = functor.map(container)(_*10)

  println( do10x[List](List(10,20,30)) ) //List(100, 200, 300)
  val tree= Tree.branch(10,Tree.leaf(20),Tree.leaf(30))
  println(  do10x(tree)  ) //Branch(100,Leaf(200),Leaf(300))

  //extension method - map
  import cats.syntax.functor._
  val incTree=tree.map(_+1)
  //shorter version
  def do10xV2[F[_]:Functor](container: F[Int]):F[Int] = container.map(_*10)

}
