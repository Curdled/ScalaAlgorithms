/**
 * Created by joe on 29/09/15.
 */
sealed trait Node[+A] {
  def isBranch: Boolean

  def insert[B >: A](ele: B)(implicit ord: math.Ordering[B]): Node[B] = {
    this match {
      case Leaf          => Branch(ele, Leaf, Leaf)
      case Branch(v,l,r) =>
        if (ord.lt(v, ele)) Branch(v,l,r insert ele)
        else                Branch(v,l insert ele,r)
    }
  }

  def merge[B >: A](that: Node[B])(implicit ord: math.Ordering[B]): Node[B] = {
    that match {
      case Leaf          => this
      case Branch(v,l,r) => this.merge(l).merge(r).insert(v)(ord)
    }
  }

  def map[B](f : A => B): Node[B] = {
    this match {
      case Leaf          => Leaf
      case Branch(v,l,r) => Branch(f(v), l map f, r map f)
    }
  }

  def flatten: Seq[A] = {
    this match {
      case Leaf          => Seq()
      case Branch(v,l,r) => l.flatten ++ (v +: r.flatten)
    }
  }

//  def flatMap[B](f: A => B) = {
//    this.map(f).flatten
//  }

  def flatMap[B](f: A => B): Seq[B] = {
    this match {
      case Leaf          => Seq()
      case Branch(v,l,r) => l.flatMap(f) ++ (f(v) +: r.flatMap(f))
    }
  }

  def inOrderFold[B](acc: B)(f: (A, B) => B): B = {
    this match {
      case Leaf          => acc
      case Branch(v,l,r) => r.inOrderFold(f(v,l.inOrderFold(acc)(f)))(f)
    }
  }

  def spaceChar = " "
  def print(indent: Int): String
}

object Leaf extends Node[Nothing]{
  override def isBranch = false

  override def print(indent: Int): String = spaceChar * indent + "."


  override def toString: String = {
    "."
  }
}

case class Branch[A](value: A, left: Node[A], right: Node[A]) extends Node[A] {
  override def isBranch = true


  def print(indent: Int): String ={
    spaceChar * indent + value + "\n" + left.print(indent+1) + "\n" + right.print(indent+1)
  }
  override def toString: String = {
    print(0)
  }
}
