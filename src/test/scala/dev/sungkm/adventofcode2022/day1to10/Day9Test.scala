package dev.sungkm.adventofcode2022
package day1to10

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day9Test extends HedgehogSuite:

  val input = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

  test("part1") {
    withMunitAssertions { a =>
      a.assertEquals(Day9.part1(input), BigInt(13))
    }
  }

  test("part2 - 1") {
    withMunitAssertions { a =>
      a.assertEquals(Day9.part2(input), BigInt(1))
    }
  }

  test("part2 - 2") {
    withMunitAssertions { a =>
      val largerInput = """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"""
      a.assertEquals(Day9.part2(largerInput), BigInt(36))
    }
  }
