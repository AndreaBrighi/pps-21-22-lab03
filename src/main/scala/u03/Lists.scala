package u03

import u02.Optionals.Option.*
import u02.Optionals.Option
import u03.Lists.List.{Cons, Nil}
import u02.Modules.Person.Teacher
import u02.Modules.{Person, isStudent}

import scala.annotation.tailrec

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(i => Cons(mapper(i), Nil()))

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = flatMap(l1)(i => pred(i) match
      case true => Cons(i, Nil())
      case _ => Nil())

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n - 1)
      case Cons(h, t) => Cons(h, t)
      case _ => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Cons(lh, lt), r) => Cons(lh, append(lt, r))
      case (Nil(), r) => r

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    def max(l: List[Int]): Option[Int] =
      @tailrec
      def _max(l: List[Int], max: Int): Int = l match
        case Cons(h, t) if max < h => _max(t, h)
        case Cons(h, t) => _max(t, max)
        case _ => max

      l match
        case Cons(h, t) => Some(_max(t, h))
        case _ => None[Int]()

    @tailrec
    def foldLeft[A, B](l: List[B])(init: A)(f: (A, B) => A): A = l match
      case Cons(h, t) => foldLeft(t)(f(init, h))(f)
      case Nil() => init

    def foldRight[A, B](l: List[B])(init: A)(f: (B, A) => A): A = l match
      case Cons(h, t) => f(h,foldRight(t)(init)(f))
      case Nil() => init

    def onlyCourses(l: List[Person]): List[String] = List.map(List.filter(l)(p => !isStudent(p)))(t => t match
      case Teacher(_, c) => c
    )

    def onlyCourses2(l: List[Person]): List[String] = List.flatMap(l)(t => t match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    )


  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
