/**
 * Day 5: Cafeteria
 *
 * Inventory database with fresh ingredient ID ranges and available IDs.
 * Part 1: Count available IDs that fall within any fresh range.
 * Part 2: Count total unique IDs covered by all fresh ranges.
 * Algorithm: Discrete interval union using Diet (balanced tree of disjoint intervals).
 */

//> using scala 3.3
//> using dep org.typelevel::cats-collections-core:0.9.10

import cats.collections.{Diet, Range}
import cats.syntax.all.*

object Day5:

  extension (r: Range[Long])
    def size: Long = r.end - r.start + 1

  private val RangeRe = """(\d+)-(\d+)""".r

  def parse(input: String): (Diet[Long], List[Long]) =
    val Array(ranges, ids) = input.split("\n\n")
    val diet = ranges.linesIterator.toList.foldMap { case RangeRe(s, e) =>
      Diet.fromRange(Range(s.toLong, e.toLong))
    }
    (diet, ids.linesIterator.flatMap(_.toLongOption).toList)

  def solve(input: String): Int =
    val (diet, ids) = parse(input)
    ids.count(diet.contains)

  def solve2(input: String): Long =
    parse(input)._1.foldLeftRange(0L)(_ + _.size)

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve(input))
    println(solve2(input))
