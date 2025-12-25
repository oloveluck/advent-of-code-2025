/**
 * Day 6: Trash Compactor
 *
 * Cephalopod math worksheet with vertical arithmetic problems.
 * Part 1: Numbers read horizontally per row, left-to-right.
 * Part 2: Numbers read vertically per column (base-10 polynomial), right-to-left.
 * Algorithm: Transpose grid, partition by space columns, fold with selected monoid.
 */

//> using scala 3.3
//> using dep org.typelevel::cats-collections-core:0.9.10

import cats.syntax.all.*

object Day6:

  type Problem = Vector[String]

  def toNumber(s: String): BigInt =
    s.filter(_.isDigit).foldLeft(BigInt(0))(_ * 10 + _.asDigit)

  def eval(op: Char)(nums: Seq[BigInt]): BigInt = op match
    case '+' => nums.sum
    case '*' => nums.product

  extension (p: Problem)
    def operator: Char = p.flatMap(_.lastOption).find(!_.isSpaceChar).get
    def data: Problem = p.map(_.init)

  def parseProblems(input: String): Vector[Problem] =
    val lines = input.linesIterator.toVector
    val width = lines.map(_.length).max
    val cols = lines.map(_.padTo(width, ' ')).transpose.map(_.mkString)

    Vector.unfold(cols) { rem =>
      val trimmed = rem.dropWhile(_.forall(_ == ' '))
      Option.when(trimmed.nonEmpty)(trimmed.span(!_.forall(_ == ' ')))
    }

  def solveWith(input: String)(extract: Problem => Seq[BigInt]): BigInt =
    parseProblems(input).foldMap(p => eval(p.operator)(extract(p.data)))

  def solve(input: String): BigInt = solveWith(input) { cols =>
    cols.map(_.toVector).transpose.map(_.mkString.trim).flatMap(_.toLongOption).map(BigInt(_))
  }

  def solve2(input: String): BigInt = solveWith(input)(_.reverse.map(toNumber))

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve(input))
    println(solve2(input))
