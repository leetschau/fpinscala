sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this.map(x => Some(x)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a)
                    else None
  }

  def mean(xs: Seq[Double]): Option[Double] = 
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(math.pow(_ - m, 2))))
    //if (xs.isEmpty) None
    //else Some(xs.map(math.pow(_ - xs.mean(), 2)) / xs.length)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Main extends App{
  assert(Some(3).map(_ * 2) == Some(6))
  assert(Some(3).getOrElse(4) == 3)
  assert(None.getOrElse(5) == 5)
  assert(Some(8).filter(_ > 5) == Some(8))
  assert(Some(7).flatMap(x => if (x > 23) Some(x + 5) else None) == None)
  assert(Some(37).flatMap(x => if (x > 23) Some(x + 5) else None) == Some(42))
  assert(Some(37).orElse(Some(74)) == Some(37))
  assert(None.orElse(Some(74)) == Some(74))
}
