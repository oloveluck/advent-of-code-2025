/**
 * Day 10: Factory
 *
 * Part 1: Linear algebra over GF(2) - buttons XOR lights.
 * Part 2: Linear algebra over ℤ - solve Ax = b, minimize L1 norm.
 *
 * Structure: Gaussian elimination over ℚ, null space enumeration.
 */

//> using scala 3.3
//> using dep org.typelevel::spire:0.18.0

import spire.math.Rational
import spire.implicits.*

object Day10:

  type R = Rational
  type Vec = Vector[R]
  type Mat = Vector[Vec]

  val zero: R = Rational.zero
  val one: R = Rational.one

  case class Machine(lights: Long, buttons: Vector[Set[Int]], joltages: Vector[Int]):
    def buttonMasks: Vector[Long] = buttons.map(_.foldLeft(0L)((acc, i) => acc | (1L << i)))

  def parse(input: String): Vector[Machine] =
    val lightPat = """\[([.#]+)\]""".r
    val btnPat = """\(([0-9,]*)\)""".r
    val joltPat = """\{([0-9,]+)\}""".r

    input.linesIterator.filter(_.nonEmpty).map { line =>
      val lights = lightPat.findFirstMatchIn(line).get.group(1)
        .zipWithIndex.foldLeft(0L) { case (acc, (c, i)) => if c == '#' then acc | (1L << i) else acc }
      val buttons = btnPat.findAllMatchIn(line).map { m =>
        if m.group(1).isEmpty then Set.empty[Int] else m.group(1).split(",").map(_.toInt).toSet
      }.toVector
      val joltages = joltPat.findFirstMatchIn(line).get.group(1).split(",").map(_.toInt).toVector
      Machine(lights, buttons, joltages)
    }.toVector

  def solve(input: String): Long = parse(input).map { m =>
    val target = m.lights
    val btns = m.buttonMasks
    (0 until (1 << btns.length))
      .filter(mask => btns.indices.foldLeft(0L)((acc, i) => if (mask & (1 << i)) != 0 then acc ^ btns(i) else acc) == target)
      .map(Integer.bitCount)
      .min
  }.sum

    val (n, k) = (A.length, A.headOption.map(_.length).getOrElse(0))

    // Gaussian elimination → (RREF, transformed b, pivot columns)
    lazy val (rref, bTrans, pivots): (Mat, Vec, Vector[Int]) =
      val aug0 = A.zip(b).map((row, bi) => row :+ bi)
      val (aug, pivs, _) = (0 until k).foldLeft((aug0, Vector.empty[Int], 0)) { case ((aug, pivots, row), col) =>
        if row >= n then (aug, pivots, row)
        else (row until n).find(r => aug(r)(col) != zero) match
          case None => (aug, pivots, row)
          case Some(pr) =>
            val swapped = aug.updated(row, aug(pr)).updated(pr, aug(row))
            val scaled = swapped.updated(row, swapped(row).map(_ / swapped(row)(col)))
            val eliminated = (0 until n).foldLeft(scaled) { (m, r) =>
              if r == row || m(r)(col) == zero then m
              else m.updated(r, m(r).zip(m(row)).map((a, b) => a - m(r)(col) * b))
            }
            (eliminated, pivots :+ col, row + 1)
      }
      (aug.map(_.dropRight(1)), aug.map(_.last), pivs)

    lazy val nullBasis: Vector[Vec] =
      val freeVars = (0 until k).filterNot(pivots.contains)
      freeVars.map { freeCol =>
        Vector.tabulate(k) { j =>
          if j == freeCol then one
          else pivots.indexOf(j) match
            case -1 => zero
            case row => -rref(row)(freeCol)
        }
      }.toVector

    lazy val particular: Option[Vec] =
      if bTrans.drop(pivots.length).exists(_ != zero) then None
      else Some(Vector.tabulate(k)(j => pivots.indexOf(j) match { case -1 => zero; case row => bTrans(row) }))

    def eval(t: Vector[Int]): Option[Long] =
      particular.flatMap { x0 =>
        val x = (0 until k).map { j =>
          x0(j) + t.zip(nullBasis).map((ti, ni) => Rational(ti) * ni(j)).foldLeft(zero)(_ + _)
        }
        if x.forall(r => r >= 0 && r.isWhole) then Some(x.map(_.toLong).sum) else None
      }

    def minNonNegativeL1: Long =
      if k == 0 then return if b.forall(_ == zero) then 0L else Long.MaxValue

      particular match
        case None => Long.MaxValue
        case Some(x0) if nullBasis.isEmpty =>
          if x0.forall(r => r >= 0 && r.isWhole) then x0.map(_.toLong).sum else Long.MaxValue
        case _ =>
          var best = Long.MaxValue
          val d = nullBasis.length
          val range = if d <= 2 then -300 to 300 else if d <= 3 then -100 to 100 else -50 to 50
          def search(dim: Int, t: Vector[Int]): Unit =
            if dim == d then eval(t).foreach(s => best = math.min(best, s))
            else for ti <- range do search(dim + 1, t :+ ti)
          search(0, Vector.empty)
          best

  def solve2(input: String): Long = parse(input).map { m =>
    val A: Mat = (0 until m.joltages.length).map { i =>
      (0 until m.buttons.length).map(j => if m.buttons(j).contains(i) then one else zero).toVector
    }.toVector
    val b: Vec = m.joltages.map(Rational(_)).toVector
    LinearSystem(A, b).minNonNegativeL1
  }.sum

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve(input))
    println(solve2(input))
