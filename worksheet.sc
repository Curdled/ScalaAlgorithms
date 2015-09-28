def qsort(xs: List[Int]): List[Int] = {
  if (xs.size <= 1)
    return xs
  val left = xs.tail.filter( _ <= xs.head)
  val right = xs.tail.filter( _ > xs.head)
  qsort(left) ++ (xs.head :: qsort(right))
}

def msort(xs: List[Int], f: (Int, Int) => Boolean): List[Int] = {
  def combine(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x::xs, y::ys) =>
        if (f(x,y)) y::combine(l1, ys)
        else x::combine(xs, l2)
    }
  }
  if (xs.size <= 1) return xs
  val (left, right) = xs.splitAt(xs.size / 2)
  combine(msort(left, f), msort(right, f))
}


msort(List(1,5,2), {_ < _})


val hello = Array(Array(1,1), Array(0,0))



val g1 = Map('a' -> Set('c','b', 'u'), 'c'-> Set('a','d'), 'd'-> Set('c','b'), 'b'-> Set('e'))

val g2 = Map('+' -> Set('1','2'))

type Vertex = Char
type Graph = Map[Vertex, Set[Vertex]]

type VertexDepth = (Vertex, Int)

def dfs(start: Vertex, g: Graph, f: (List[Vertex], Vertex) => List[Vertex], acc: List[Vertex]): List[Vertex] = {
  def dfs0(toVisit: List[Vertex], visited: Set[Vertex], acc: List[Vertex]): List[Vertex] = {
    toVisit match {
      case Nil => acc
      case x::xs =>
        dfs0(g.getOrElse(x, Set()).diff(visited).diff(xs.toSet).toList ++ xs, visited + x, f(acc, x))
    }
  }
  dfs0(List(start), Set(start), List())
}

def bfs(start: Vertex, g: Graph, f: (List[Vertex], Vertex) => List[Vertex], acc: List[Vertex]): List[Vertex] = {
  def bfs0(toVisit: List[Vertex], visited: Set[Vertex], acc: List[Vertex]): List[Vertex] = {
    toVisit match {
      case Nil => acc
      case x::xs =>
        bfs0(xs ++ g.getOrElse(x, Set()).diff(visited).diff(xs.toSet), visited + x, f(acc, x))
    }
  }
  bfs0(List(start), Set(start), List())
}

def dfs2(start: Vertex, g: Graph, f: (List[Vertex], Vertex) => List[Vertex]): List[(Vertex, Int)] = {
  def dfs0(toVisit: List[Vertex], visited: Set[Vertex], d: Int): List[(Vertex, Int)] = {
    toVisit match {
      case Nil => List()
      case x::xs =>
        (x,d) :: dfs0(g.getOrElse(x, Set()).diff(visited).toList, visited + x, d+1) ++ dfs0(xs, visited + x, d)
    }
  }
  val l: List[(Vertex, Int)] = List()
  dfs0(List(start), Set(start), 0).foldRight(l) {
    (v, acc) => acc.find(e => e._1 == v._1) match {
      case None => v::acc
      case Some(x) =>
        if (x._2 > v._2) v :: acc.diff(List(x))
        else acc
    }
  }
}

def bfs2(start: Vertex, g: Graph, f: (List[VertexDepth], VertexDepth) => List[VertexDepth], acc: List[VertexDepth]): List[VertexDepth] = {
  def bfs0(toVisit: List[VertexDepth], visited: Set[Vertex], acc: List[VertexDepth]): List[VertexDepth] = {
    toVisit match {
      case Nil => acc
      case x::xs =>
        bfs0(xs ++ g.getOrElse(x._1, Set()).diff(visited).diff(xs.map(v => v._1).toSet).map(v => (v, x._2 + 1)), visited + x._1, f(acc, x))
    }
  }
  bfs0(List((start, 0)), Set(start), List())
}

def f(l: List[Vertex], i: Vertex) = {
  l ++ List(i)
}

def f2(l: List[VertexDepth], i: VertexDepth) = {
  l ++ List(i)
}





bfs('+', g2, f, List())
dfs('+', g2, f, List())

dfs2('a', g1, f)
bfs2('a', g1, f2, List())
