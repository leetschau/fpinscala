//package fpinscala.datastructures

/**
 * How to load fpinscala.datastructures.List into REPL?
 *
 * See note "Scala Development on Linux"
 */
sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  // answer: 3 (the third case, x = 1, y = 2)

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.7
  def product3(ns: List[Double]) =
    foldRight(ns, 1.0)((x,y) => if (x==0) 0 else (x*y))

  // Exercise 3.8
  // scala> List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  // res6: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, xs)
    }
    /* the official answer use case guard, which is more concise:
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
    */ 

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  // Exercise 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x,y) => 1 + y)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 3.11
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_+_)
  def product4(ns: List[Double]) = foldLeft(ns, 1.0)(_*_)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((x,y) => Cons(y, x))

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  // Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def concat2[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => foldLeft(xs, x)((x,y) => append2(x, y))
  }

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  // Exercise 3.17
  def double2Str(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString(), double2Str(xs))
  }

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f))
                        else filter(xs)(f)
  }

  // Exercise 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  // Exercise 3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  // Exercise 3.22
  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    case Nil => Nil
    case Cons(x, xs) => l2 match {
      case Nil => Nil
      case Cons(y, ys) => Cons(x + y, zipAdd(xs, ys))
    }
  }

  // Exercise 3.23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    l1 match {
      case Nil => Nil
      case Cons(x, xs) => l2 match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
      }
    }

  def zipWith2[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  def test() = {
    assert(List.addOne(List(1, 2, 3)) == List(2, 3, 4))
    assert(List.double2Str(List(1.0, 2.73, 3.23)) ==
            List("1.0", "2.73", "3.23"))
    assert(List.map(List(1.0,2.73,3.23))(_.toString()) ==
            List("1.0", "2.73", "3.23"))
    assert(List.map(List(1.0,2.73,3.23))(_ * 2) == List(2.0, 5.46, 6.46))
    assert(List.filter(List(1.0, 2.73, 3.23))(_ > 2) == List(2.73, 3.23))
    assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) ==
            List(1, 1, 2, 2, 3, 3))
    assert(List.filter2(List(1.0, 2.73, 3.23))(_ > 2) == List(2.73, 3.23))
    assert(List.zipAdd(List(1, 2, 3), List(10, 20, 30)) == List(11, 22, 33))
    assert(List.zipWith(List(1, 2, 3), List(1.5, 2.2, 3.3))((x, y) => x + y * 2)
      == List(4.0, 6.4, 9.6))
    assert(List.zipWith2(List(1, 2, 3), List(1.5, 2.2, 3.3))((x, y) => x + y * 2)
      == List(4.0, 6.4, 9.6))
  }
}
