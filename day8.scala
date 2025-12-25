/**
 * Day 8: Playground
 *
 * Kruskal's MST on junction boxes in metric space.
 * Part 1: Product of 3 largest components after 1000 edges.
 * Part 2: X-product of final spanning tree edge.
 *
 * Structure: Metric typeclass, State monad over UF, coalgebra emission.
 */

//> using scala 3.3
//> using dep org.typelevel::cats-core:2.10.0

import cats.data.State
import cats.syntax.all.*

object Day8:

  trait Metric[A]:
    extension (a: A) def distSq(b: A): Long

  case class Point(x: Long, y: Long, z: Long)

  given Metric[Point] with
    extension (p: Point) def distSq(q: Point): Long =
      val (dx, dy, dz) = (p.x - q.x, p.y - q.y, p.z - q.z)
      dx * dx + dy * dy + dz * dz
  case class UF(parent: Vector[Int], rank: Vector[Int], size: Vector[Int]):
    def components: Int = parent.indices.count(i => parent(i) == i)
    def componentSizes: Seq[Int] = parent.indices.filter(i => parent(i) == i).map(size)

  object UF:
    def apply(n: Int): UF = UF(Vector.tabulate(n)(identity), Vector.fill(n)(0), Vector.fill(n)(1))

    def find(x: Int): State[UF, Int] = State { uf =>
      def go(i: Int, path: List[Int]): (Int, List[Int]) =
        if uf.parent(i) == i then (i, path)
        else go(uf.parent(i), i :: path)
      val (root, path) = go(x, Nil)
      val compressed = uf.copy(parent = path.foldLeft(uf.parent)(_.updated(_, root)))
      (compressed, root)
    }

    def tryUnion(a: Int, b: Int): State[UF, Boolean] = for
      ra <- find(a)
      rb <- find(b)
      merged <- State[UF, Boolean] { uf =>
        if ra == rb then (uf, false)
        else
          val newUF = if uf.rank(ra) < uf.rank(rb) then
            uf.copy(parent = uf.parent.updated(ra, rb), size = uf.size.updated(rb, uf.size(ra) + uf.size(rb)))
          else if uf.rank(ra) > uf.rank(rb) then
            uf.copy(parent = uf.parent.updated(rb, ra), size = uf.size.updated(ra, uf.size(ra) + uf.size(rb)))
          else
            uf.copy(parent = uf.parent.updated(rb, ra), rank = uf.rank.updated(ra, uf.rank(ra) + 1),
                    size = uf.size.updated(ra, uf.size(ra) + uf.size(rb)))
          (newUF, true)
      }
    yield merged

  case class Edge(dist: Long, i: Int, j: Int)
  given Ordering[Edge] = Ordering.by(_.dist)

  def processEdge(e: Edge): State[UF, Option[(Int, Int)]] =
    UF.tryUnion(e.i, e.j).map(if _ then Some((e.i, e.j)) else None)

  def kruskal[A: cats.Monoid](edges: Seq[Edge], n: Int)(
    emit: (UF, Option[(Int, Int)]) => A,
    stop: UF => Boolean = _ => false
  ): (UF, A) =
    edges.foldLeft((UF(n), cats.Monoid[A].empty)) { case ((uf, acc), edge) =>
      if stop(uf) then (uf, acc)
      else
        val (uf2, merged) = processEdge(edge).run(uf).value
        (uf2, acc |+| emit(uf2, merged))
    }

  def parse(input: String): Vector[Point] =
    input.linesIterator.map { line =>
      val Array(x, y, z) = line.split(",").map(_.trim.toLong)
      Point(x, y, z)
    }.toVector

  def edges[A: Metric](points: Vector[A]): Seq[Edge] =
    (for i <- points.indices; j <- i + 1 until points.length
     yield Edge(points(i).distSq(points(j)), i, j)).sorted

  def solve(input: String): Long =
    val points = parse(input)
    val (uf, _) = kruskal[Unit](edges(points).take(1000), points.length)((_, _) => (), _ => false)
    uf.componentSizes.sorted.takeRight(3).map(_.toLong).product

  def solve2(input: String): Long =
    val points = parse(input)
    given cats.Monoid[Option[(Int, Int)]] = cats.Monoid.instance(None, (a, b) => b.orElse(a))
    val (_, lastMerge) = kruskal(edges(points), points.length)(
      (_, merged) => merged,
      _.components == 1
    )
    lastMerge.map((i, j) => points(i).x * points(j).x).getOrElse(0L)

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve(input))
    println(solve2(input))
