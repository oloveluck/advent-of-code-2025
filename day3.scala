/**
 * Day 3: Lobby
 *
 * Batteries with joltage ratings 1-9 to power an escalator.
 * Part 1: Select k=12 digits to form the largest number (preserving order).
 * Algorithm: Greedy selection from shrinking window.
 */

//> using scala 3.3
//> using dep org.typelevel::cats-core:2.10.0
//> using dep org.typelevel::spire:0.18.0

import cats.syntax.all.*
import spire.math.SafeLong

object Day3:

  def maxJoltageK(digits: Vector[Int], k: Int): SafeLong =
    List.unfold((digits, k)) { case (rem, needed) =>
      Option.when(needed > 0) {
        val window = rem.take(rem.length - needed + 1)
        val (maxVal, maxIdx) = window.zipWithIndex.maxBy(_._1)
        (maxVal, (rem.drop(maxIdx + 1), needed - 1))
      }
    }.foldLeft(SafeLong.zero)(_ * 10 + _)

  def solve(input: String): SafeLong =
    input.linesIterator
      .filter(_.nonEmpty)
      .map(line => maxJoltageK(line.map(_.asDigit).toVector, 12))
      .foldLeft(SafeLong.zero)(_ + _)

  @main def run(): Unit =
    println(solve(scala.io.Source.stdin.mkString))
