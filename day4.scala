/**
 * Day 4: Printing Department
 *
 * Paper rolls (@) on a grid; forklifts can access rolls with < 4 neighbors.
 * Part 1: Count initially accessible rolls.
 * Part 2: Iteratively remove accessible rolls until stable; count total removed.
 * Algorithm: Moore neighborhood (8-connectivity), fixed-point iteration.
 */

//> using scala 3.3
//> using dep org.typelevel::cats-core:2.10.0

import cats.syntax.all.*

object Day4:

  case class Pos(r: Int, c: Int):
    def +(other: Pos): Pos = Pos(r + other.r, c + other.c)
    def neighbors: List[Pos] = Pos.directions.map(this + _)

  object Pos:
    val directions: List[Pos] =
      (List(-1, 0, 1), List(-1, 0, 1)).tupled.filter(_ != (0, 0)).map(Pos.apply)

  @annotation.tailrec
  def fix[A](f: A => A)(a: A): A =
    val next = f(a)
    if next == a then a else fix(f)(next)

  def parseGrid(input: String): Set[Pos] =
    input.linesIterator.zipWithIndex.flatMap { (line, r) =>
      line.zipWithIndex.collect { case ('@', c) => Pos(r, c) }
    }.toSet

  def accessible(rolls: Set[Pos]): Set[Pos] =
    rolls.filter(_.neighbors.count(rolls) < 4)

  def solve(input: String): Int = accessible(parseGrid(input)).size
  def solve2(input: String): Int =
    val init = parseGrid(input)
    init.size - fix((s: Set[Pos]) => s -- accessible(s))(init).size

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve(input))
    println(solve2(input))
