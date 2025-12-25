//> using scala 3.3
//> using dep org.typelevel::cats-core:2.10.0

import cats.syntax.all.*

object Day12:

  case class Pt(r: Int, c: Int):
    def +(o: Pt): Pt = Pt(r + o.r, c + o.c)
    def rot90: Pt = Pt(c, -r)
    def flip: Pt = Pt(r, -c)

  type Shape = Set[Pt]

  extension (s: Shape)
    def normalize: Shape =
      val origin = Pt(s.map(_.r).min, s.map(_.c).min)
      s.map(p => Pt(p.r - origin.r, p.c - origin.c))

    def at(offset: Pt): Shape = s.map(_ + offset)

    def variants: Set[Shape] =
      val rots = Iterator.iterate(s)(_.map(_.rot90)).take(4).toSet
      (rots ++ rots.map(_.map(_.flip))).map(_.normalize)

  case class Region(w: Int, h: Int, needs: List[Set[Shape]]):
    def positions: Seq[Pt] = for r <- 0 until h; c <- 0 until w yield Pt(r, c)
    def inBounds(s: Shape): Boolean = s.forall(p => p.r >= 0 && p.r < h && p.c >= 0 && p.c < w)
    def area: Int = w * h

  def canFit(region: Region): Boolean =
    val shapes = region.needs.sortBy(_.size)
    if shapes.foldMap(_.head.size) > region.area then false
    else
      def go(occupied: Shape, remaining: List[Set[Shape]]): Boolean =
        remaining match
          case Nil => true
          case variants :: rest =>
            (for
              shape  <- variants.iterator
              pos    <- region.positions.iterator
              placed  = shape.at(pos)
              if region.inBounds(placed) && (placed & occupied).isEmpty
            yield go(occupied | placed, rest)).exists(identity)

      go(Set.empty, shapes)

  def parseShape(lines: Seq[String]): Shape =
    (for
      (line, r) <- lines.zipWithIndex
      (ch, c)   <- line.zipWithIndex if ch == '#'
    yield Pt(r, c)).toSet

  def parse(input: String): Vector[Region] =
    val sections = input.split("\n\n").toVector

    val shapeVariants = sections.init.mkString("\n\n")
      .split("\n(?=\\d+:)").toVector
      .map(block => parseShape(block.linesIterator.drop(1).toSeq).variants)

    sections.last.linesIterator.map { line =>
      val Array(dims, counts) = line.split(": ")
      val Array(w, h) = dims.split("x").map(_.toInt)
      val needs = counts.split(" ").map(_.toInt).toVector
        .zipWithIndex.flatMap((qty, i) => List.fill(qty)(shapeVariants(i))).toList
      Region(w, h, needs)
    }.toVector

  def solve(input: String): Int = parse(input).count(canFit)

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve(input))
