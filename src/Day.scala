package aoc

import scala.io.Source
import scala.util.Using

trait Day:
  def day: Int

  def part1(input: List[String]): Any
  def part2(input: List[String]): Any

  def inputFile: String = f"inputs/day$day%02d.txt"

  def readInput: List[String] =
    Using.resource(Source.fromFile(inputFile))(_.getLines().toList)

  def readInputRaw: String =
    Using.resource(Source.fromFile(inputFile))(_.mkString)

  def main(args: Array[String]): Unit =
    val input = readInput
    println(s"=== Day $day ===")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")

  // String parsing utilities
  extension (s: String)
    def splitTrim(sep: String): List[String] =
      s.split(sep).map(_.trim).filter(_.nonEmpty).toList

    def ints: List[Int] = "-?\\d+".r.findAllIn(s).map(_.toInt).toList
    def longs: List[Long] = "-?\\d+".r.findAllIn(s).map(_.toLong).toList

  // Grid utilities
  def parseGrid(input: List[String]): Map[(Int, Int), Char] =
    for
      (row, y) <- input.zipWithIndex.toMap
      (char, x) <- row.zipWithIndex
    yield (x, y) -> char

  def neighbors4(x: Int, y: Int): List[(Int, Int)] =
    List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))

  def neighbors8(x: Int, y: Int): List[(Int, Int)] =
    for
      dx <- List(-1, 0, 1)
      dy <- List(-1, 0, 1)
      if dx != 0 || dy != 0
    yield (x + dx, y + dy)
