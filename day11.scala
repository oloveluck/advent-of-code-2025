/**
 * Day 11: Reactor
 *
 * Count paths through a directed graph of devices.
 * Part 1: All paths from "you" to "out".
 * Part 2: Paths from "svr" to "out" visiting both "dac" and "fft".
 * Algorithm: Memoized DFS with state tracking via foldMap.
 */

//> using scala 3.3
//> using dep org.typelevel::cats-core:2.10.0

import cats.syntax.all.*

object Day11:

  type Graph = Map[String, List[String]]

  def parse(input: String): Graph =
    input.linesIterator.map(_.split(": ")).map(a => a(0) -> a(1).split(" ").toList).toMap

  def countPaths[S](graph: Graph, start: String, init: S)(
    step: (String, S) => S,
    accept: S => Boolean
  ): Long =
    val memo = collection.mutable.Map.empty[(String, S), Long]

    def go(node: String, state: S): Long =
      val s = step(node, state)
      if node == "out" then if accept(s) then 1L else 0L
      else memo.getOrElseUpdate((node, s),
        graph.getOrElse(node, Nil).foldMap(go(_, s))
      )

    go(start, init)

  def solve(input: String): Long =
    countPaths(parse(input), "you", ())(
      step   = (_, _) => (),
      accept = _ => true
    )

  def solve2(input: String): Long =
    countPaths(parse(input), "svr", 0)(
      step   = (n, s) => s | (if n == "dac" then 1 else if n == "fft" then 2 else 0),
      accept = _ == 3
    )

  @main def run(): Unit =
    val input = scala.io.Source.stdin.mkString
    println(solve(input))
    println(solve2(input))
