package com.chatwork.quiz.collection

import com.chatwork.quiz.MyOption

sealed trait MyList[+A] {

  // Easy
  def length: Int = foldLeft(0)((s, _) => s + 1)

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case MyCons(h, t) => t.foldLeft(f(z, h))(f)
    case MyNil => z
  }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B = this.reverse.foldLeft(z)((s, x) => f(x, s)) // todo 引数名てきとうすぎる

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] = MyCons(b, this)

  // scalastyle:on

  // Normal
  def reverse: MyList[A] = foldLeft[MyList[A]](MyNil)((s, x) => MyCons(x, s))

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] = foldRight(b)((l, r) => MyCons(l, r))

  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] = ???

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] = ???

  // Normal
  def filter(f: A => Boolean): MyList[A] = ???

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyList[A] = ???

  // Normal
  def find(f: A => Boolean): MyOption[A] = ???

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = ???

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] = as.headOption match {
    case Some(h) => MyCons(h, apply(as.tail: _*))
    case _ => MyNil
  }

}
