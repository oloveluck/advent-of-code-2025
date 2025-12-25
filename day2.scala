//> using scala 3.3

/**
 * Day 2: Gift Shop
 *
 * Invalid ID = base × repunit(n, k) where repunit(n, k) = (10^n - 1) / (10^k - 1)
 * Part 1: r = 2 only (n = 2k)
 * Part 2: any r >= 2
 */
object Day2:

  def parseRanges(s: String): Array[(BigInt, BigInt)] =
    s.split(",").map(_.split("-")).map(a => (BigInt(a(0)), BigInt(a(1))))

  def repunit(n: Int, k: Int): BigInt =
    (BigInt(10).pow(n) - 1) / (BigInt(10).pow(k) - 1)

  def invalidIds(lo: BigInt, hi: BigInt)(pairs: Int => Iterator[Int]): Iterator[BigInt] =
    val maxDigits = hi.toString.length
    (2 to maxDigits).iterator.flatMap { n =>
      pairs(n).flatMap { k =>
        val mult = repunit(n, k)
        val minBase = if k == 1 then BigInt(1) else BigInt(10).pow(k - 1)
        val maxBase = BigInt(10).pow(k) - 1
        val baseLo = ((lo + mult - 1) / mult) max minBase
        val baseHi = (hi / mult) min maxBase
        (baseLo to baseHi).iterator.map(_ * mult)
      }
    }

  def solve1(input: String): BigInt =
    val ranges = parseRanges(input.linesIterator.next())
    ranges.iterator.flatMap((lo, hi) =>
      invalidIds(lo, hi)(n => Iterator(n / 2).filter(k => n == 2 * k))
    ).toSet.sum

  def solve2(input: String): BigInt =
    val ranges = parseRanges(input.linesIterator.next())
    ranges.iterator.flatMap((lo, hi) =>
      invalidIds(lo, hi)(n => (1 to n / 2).iterator.filter(n % _ == 0))
    ).toSet.sum

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve1(input))
    println(solve2(input))
