/**
 * Created by joe on 29/09/15.
 */
object Sorting {
  def qsort[A](xs: Seq[A])(implicit ord: math.Ordering[A]): Seq[A] = {
    if (xs.size <= 1)
      return xs
    val left = xs.tail.filter( ord.lteq(_, xs.head))
    val right = xs.tail.filter( ord.gt(_, xs.head))
    qsort(left) ++ (xs.head +: qsort(right))
  }

  def msort[A](xs: Seq[A])(implicit ord: math.Ordering[A]): Seq[A] = {
    def combine(l1: Seq[A], l2: Seq[A]): Seq[A] = {
      (l1, l2) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x::xs, y::ys) =>
          if (ord.gteq(x,y)) y+:combine(l1, ys)
          else x+:combine(xs, l2)
      }
    }
    if (xs.size <= 1) return xs
    val (left, right) = xs.splitAt(xs.size / 2)
    combine(msort(left), msort(right))
  }
}
