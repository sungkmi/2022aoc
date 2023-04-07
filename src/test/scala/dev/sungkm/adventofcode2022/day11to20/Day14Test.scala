package dev.sungkm.adventofcode2022
package day11to20

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day14Test extends HedgehogSuite:

  val input = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""

  test("part1"):
    withMunitAssertions: a =>
      a.assertEquals(Day14.part1(input), BigInt(24))


  test("vertical fall"):
    withMunitAssertions: a =>
      val cm = Day14.CaveMap.parse(input)
      a.assertEquals(cm.verticalFall(492, 10), None)


  test("part2"):
    withMunitAssertions: a =>
      a.assertEquals(Day14.part2(input), BigInt(93))

