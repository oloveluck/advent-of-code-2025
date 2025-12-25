/**
 * Day 9: Movie Theater
 *
 * Part 1: Largest rectangle with red tiles at opposite corners.
 * Part 2: Rectangle must be entirely inside the rectilinear polygon.
 *
 * Structure: Spire Interval algebra, Rect = Interval × Interval.
 */

//> using scala 3.3
//> using dep org.typelevel::spire:0.18.0

import spire.math.Interval
import spire.implicits.*

object Day9:

  case class Point(x: Long, y: Long)

  // Rectangle as product of intervals (store bounds explicitly for convenience)
  case class Rect(xLo: Long, xHi: Long, yLo: Long, yHi: Long):
    def x: Interval[Long] = Interval(xLo, xHi)
    def y: Interval[Long] = Interval(yLo, yHi)

    def corners: List[Point] = for
      px <- List(xLo, xHi)
      py <- List(yLo, yHi)
    yield Point(px, py)

    def area: Long = (xHi - xLo + 1) * (yHi - yLo + 1)

    // Open interior for strict containment checks
    def openX: Interval[Long] = Interval.open(xLo, xHi)
    def openY: Interval[Long] = Interval.open(yLo, yHi)

  object Rect:
    def fromCorners(a: Point, b: Point): Rect =
      Rect(math.min(a.x, b.x), math.max(a.x, b.x),
           math.min(a.y, b.y), math.max(a.y, b.y))

  // Rectilinear polygon with containment predicates
  case class Polygon(vertices: Vector[Point]):
    lazy val edges: Vector[(Point, Point)] =
      vertices.indices.map(i => (vertices(i), vertices((i + 1) % vertices.length))).toVector

    // Point on boundary (edge or vertex)
    private def onBoundary(p: Point): Boolean = edges.exists { case (a, b) =>
      if a.x == b.x then
        p.x == a.x && Interval(math.min(a.y, b.y), math.max(a.y, b.y)).contains(p.y)
      else
        p.y == a.y && Interval(math.min(a.x, b.x), math.max(a.x, b.x)).contains(p.x)
    }

    // Ray casting: count vertical edge crossings to the right
    private def rayCrossings(p: Point): Int = edges.count { case (a, b) =>
      a.x == b.x && a.x > p.x &&
        Interval.openUpper(math.min(a.y, b.y), math.max(a.y, b.y)).contains(p.y)
    }

    def contains(p: Point): Boolean =
      onBoundary(p) || rayCrossings(p) % 2 == 1

    // Edge cuts strictly through rectangle interior
    private def edgeCutsThrough(r: Rect): Boolean = edges.exists { case (a, b) =>
      if a.x == b.x then // vertical edge at x = a.x
        r.openX.contains(a.x) &&
          Interval(math.min(a.y, b.y), math.max(a.y, b.y)).intersects(r.openY)
      else // horizontal edge at y = a.y
        r.openY.contains(a.y) &&
          Interval(math.min(a.x, b.x), math.max(a.x, b.x)).intersects(r.openX)
    }

    def contains(r: Rect): Boolean =
      r.corners.forall(contains) && !edgeCutsThrough(r)

  def parse(input: String): Vector[Point] =
    input.linesIterator.map { line =>
      val Array(x, y) = line.split(",").map(_.trim.toLong)
      Point(x, y)
    }.toVector

  def solve(input: String): Long =
    val points = parse(input)
    (for
      i <- points.indices
      j <- i + 1 until points.length
    yield Rect.fromCorners(points(i), points(j)).area).max

  def solve2(input: String): Long =
    val polygon = Polygon(parse(input))
    val n = polygon.vertices.length
    (for
      i <- 0 until n
      j <- i + 1 until n
      rect = Rect.fromCorners(polygon.vertices(i), polygon.vertices(j))
      if polygon.contains(rect)
    yield rect.area).max

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve(input))
    println(solve2(input))
