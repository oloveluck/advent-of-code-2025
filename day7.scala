/**
 * Day 7: Laboratories
 *
 * Tachyon beam through a manifold with splitters (^).
 * Part 1: Count splits (hylomorphism: unfold states, fold emissions).
 * Part 2: Count timelines (anamorphism to terminal state).
 *
 * Structure: Coalgebra S → (A, S) emitting A at each step.
 */

//> using scala 3.3
//> using dep org.typelevel::cats-core:2.10.0

import cats.Monoid
import cats.syntax.all.*

object Day7:

  case class Grid(rows: Vector[String]):
    val width: Int = rows.map(_.length).max
    val start: Int = rows.head.indexOf('S')
    def apply(row: Int, col: Int): Char =
      if col < 0 || col >= width then ' ' else rows(row)(col)

  object Grid:
    def parse(input: String): Grid =
      Grid(input.linesIterator.toVector.map(_.padTo(256, ' ')))

  trait Coalgebra[S, A]:
    extension (s: S) def step(g: Grid, row: Int): (A, S)

  given Coalgebra[Set[Int], Int] with
    extension (s: Set[Int]) def step(g: Grid, row: Int): (Int, Set[Int]) =
      val (splits, next) = s.toList.foldMap { col =>
        g(row, col) match
          case '^' => (1, Set(col - 1, col + 1))
          case '.' => (0, Set(col))
          case _   => (0, Set.empty[Int])
      }
      (splits, next)

  type Timelines = Map[Int, BigInt]

  given Coalgebra[Timelines, Timelines] with
    extension (s: Timelines) def step(g: Grid, row: Int): (Timelines, Timelines) =
      val next = s.toList.foldMap { (col, n) =>
        g(row, col) match
          case '^' => Map(col - 1 -> n, col + 1 -> n)
          case '.' => Map(col -> n)
          case _   => Map.empty
      }
      (next, next)

  def hylo[S, A: Monoid](g: Grid)(init: S)(using C: Coalgebra[S, A]): A =
    (1 until g.rows.length).foldLeft((Monoid[A].empty, init)) { case ((acc, state), row) =>
      val (emission, next) = state.step(g, row)
      (acc |+| emission, next)
    }._1

  def ana[S, A](g: Grid)(init: S)(using C: Coalgebra[S, A]): S =
    (1 until g.rows.length).foldLeft(init)((s, row) => s.step(g, row)._2)

  def solve(input: String): Int =
    val g = Grid.parse(input)
    hylo[Set[Int], Int](g)(Set(g.start))

  def solve2(input: String): BigInt =
    val g = Grid.parse(input)
    ana[Timelines, Timelines](g)(Map(g.start -> BigInt(1))).values.sum

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve(input))
    println(solve2(input))
