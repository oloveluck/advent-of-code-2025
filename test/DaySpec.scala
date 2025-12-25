package aoc

import munit.FunSuite

class Day01Spec extends FunSuite:

  val sample = List("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")

  test("part1: count times dial lands on 0 after rotation"):
    assertEquals(Day01.part1(sample), 3)

  test("part2: count all clicks passing through 0"):
    assertEquals(Day01.part2(sample), 6)

class Day02Spec extends FunSuite:

  val sample = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

  test("part1: sum of invalid IDs in ranges"):
    assertEquals(Day02.part1(List(sample)), BigInt(1227775554))

  test("part2: sum of invalid IDs with any repetition"):
    assertEquals(Day02.part2(List(sample)), BigInt(4174379265L))
