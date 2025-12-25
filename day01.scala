//> using scala 3.3

/**
 * Day 1: Secret Entrance
 *
 * A dial with positions 0-99, starting at 50.
 * Part 1: Count positions landing on 0 after each rotation.
 * Part 2: Count every click that passes through 0.
 */
object Day01:

  enum Dir(val sign: Int):
    case L extends Dir(-1)
    case R extends Dir(+1)

  case class Rotation(dir: Dir, dist: Int):
    def apply(pos: Int): Int = Math.floorMod(pos + dir.sign * dist, 100)

    def zerosFrom(pos: Int): Int =
      val firstZero = Math.floorMod(-dir.sign * pos - 1, 100) + 1
      (dist + 100 - firstZero) / 100

  object Rotation:
    def parse(s: String): Rotation = Rotation(Dir.valueOf(s.take(1)), s.drop(1).toInt)

  def solve(input: String)(count: (Int, Rotation) => Int): Int =
    input.linesIterator.filter(_.nonEmpty).map(Rotation.parse).foldLeft((50, 0)) {
      case ((pos, total), rot) => (rot(pos), total + count(pos, rot))
    }._2

  def solve1(input: String): Int =
    solve(input)((pos, rot) => if rot(pos) == 0 then 1 else 0)

  def solve2(input: String): Int =
    solve(input)((pos, rot) => rot.zerosFrom(pos))

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve1(input))
    println(solve2(input))
