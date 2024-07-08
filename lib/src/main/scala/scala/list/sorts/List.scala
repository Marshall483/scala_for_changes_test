package scala.list.sorts

import scala.annotation.tailrec

sealed trait FList[+T] {
  def isEmpty: Boolean

  final def foreach(f: T => Unit): Unit = {
     @tailrec
     def work(data: FList[T]): Unit = data match {
         case Cons(v, vs) => {
             f(v)
             work(vs)
         }
         case _ => {}
     }
     work(this)
  }

  override def equals(that: Any): Boolean = {
    @tailrec
    def compareLists[U](first: FList[T], second: FList[U]): Boolean = (first.isEmpty, second.isEmpty) match {
      case (true, true) => true
      case (false, false) => {
          val firstCons = first.asInstanceOf[Cons[T]]
          val secondCons = second.asInstanceOf[Cons[U]]
          (firstCons.head == secondCons.head) && compareLists(firstCons.tail, secondCons.tail)
      }
      case _ => false
    }

    that match {
      case that: FList[_] => compareLists(this, that)
      case _ => false
    }
  }
}

object Nil extends FList[Nothing] {
  def isEmpty: Boolean = true
}

case class Cons[+T](head: T, tail: FList[T]) extends FList[T] {
  def isEmpty: Boolean = false
}

object FList {
    def apply[T](data: T*): FList[T] = data.foldRight[FList[T]](Nil)(Cons[T])
}
