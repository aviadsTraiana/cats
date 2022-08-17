package abstractMath
//A semigroup with empty
object Moniods {

    import  cats.Monoid
    import cats.instances.int._
    import cats.instances.string._
    import cats.instances.option._
    import cats.syntax.monoid._


    def main(args: Array[String]): Unit = {
        val str= "Hi" |+| " There!"
        println(str)
        val finalOption = Option(1) |+| Monoid[Option[Int]].empty |+| Option(5)
      println(finalOption) //Option(6)

      def combineFold[T](list: List[T])(implicit monoid: Monoid[T]):T = list.foldLeft(monoid.empty)( _ |+| _ )

      val `1+2` =combineFold(List(Option(1),Option(2),None))
      println(`1+2`)

      import cats.instances.map._
      val map1=Map(
        "Aviad" -> 20,
        "Shahar" -> 50
      )
      val map2=Map(
        "Aviad" -> 20,
        "Aviv" ->10
      )
      println(  combineFold(List(map1,map2))    ) //Map(Aviad -> 40, Aviv -> 10, Shahar -> 50)

      //shopping cart & online stores with monoids
      final case class ShoppingCart(items:List[String],total:Double)
      val shoppingCarts: List[ShoppingCart] = List(
          ShoppingCart(List("banana","router"),50),
          ShoppingCart(List("pc"),100)
      )

      import cats.instances.list._
      implicit val shoppingCartMonoid: Monoid[ShoppingCart] =
          Monoid.instance[ShoppingCart](
              ShoppingCart(List.empty,0), //empty impl
              (x, y) => ShoppingCart(x.items |+| y.items,x.total + y.total) //combine impl
          )
      def checkout(carts: List[ShoppingCart]) : ShoppingCart = combineFold(carts)

      println(   checkout(shoppingCarts)       )  //ShoppingCart(List(banana, router, pc),150.0)

    }
}
