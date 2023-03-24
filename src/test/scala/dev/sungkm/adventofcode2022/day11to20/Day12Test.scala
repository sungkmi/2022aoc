package dev.sungkm.adventofcode2022
package day11to20

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day12Test extends HedgehogSuite:

  val input = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

  test("part1"):
    withMunitAssertions: a =>
      a.assertEquals(Day12.part1(input), BigInt(31))

  test("part2"):
    withMunitAssertions: a =>
      a.assertEquals(Day12.part2(input), BigInt(29))
