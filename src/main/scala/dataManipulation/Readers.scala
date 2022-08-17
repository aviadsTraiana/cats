package dataManipulation

import cats.Id

import scala.util.Try


object Readers {

  case class Configuration(userName:String,password:String,host:String,port:String,nThreads:Int)
  case class DBConnection(userName:String,password:String){
    def getOrderStatus(orderId:Long):String ="imagine fetching status from DB"
    def getLastOrderId : Long = 456161 //imagine pulling last order id from db based on $username and password
  }
  case class HttpService(host:String,port:Int){
    def start(host:String,port:String):Unit = println(s"imagine starting a running server on $host:$port")
  }

  //bootstrapping configuration- imagine loading from config file for example
  val config: Configuration =
    Configuration("aviad","some@Password","localhost","8080",8)
  //we would like to pass those configuration around safely, with reader monad
  import cats.data.Reader
  val trySomething :Reader[Configuration,Try[Unit]] = Reader{ (c:Configuration) => Try{ print("yes!")} }
  trySomething.run(config)
  val dbReader :Reader[Configuration,DBConnection] =
    Reader(configuration => DBConnection(configuration.userName,configuration.password))
  //then apply it by injecting the config to the reader, and get the DbConnection instance already configured by using run
  val dbConn: DBConnection = dbReader.run(config)

  //or we can even continue to describe the spec of the manipulation
  val orderStatusReader : Reader[Configuration,String] = dbReader.map(conn=> conn.getOrderStatus(12))
  val aviadOrderStatus: String = orderStatusReader.run(config)

  //a reader is a monad so we can even use flatMap to chain multiple readers
  def getLastOrderStatus :Reader[Configuration,String] ={
//    val userLastOrderReader = dbReader.map(conn=> conn.getLastOrderId)
//            .flatMap(orderId=>dbReader.map(_.getOrderStatus(orderId)))
    //or with for-comprehension
    val userOrderFor : Reader[Configuration,String] = for {
      orderId <-  dbReader.map(conn=> conn.getLastOrderId)
      status <- dbReader.map(_.getOrderStatus(orderId))
    } yield status
    userOrderFor
  }

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus.run(config))
  }


}
