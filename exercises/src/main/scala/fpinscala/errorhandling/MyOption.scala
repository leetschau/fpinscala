sealed trait MyOption[+A] {
  def mymap[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }
}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

object MyOption {
  //def mymap[B](f: A => B): MyOption[B] = this match {
    //case MyNone => MyNone
    //case MySome(a) => MySome(f(a))
  //}
}


object Main extends App{
  println(MySome(3).mymap(_ * 2))
}
