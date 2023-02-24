package dev.sungkm.adventofcode2022
package day1to10

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day8Test extends HedgehogSuite:

  val input = """30373
25512
65332
33549
35390"""

  test("part1") {
    withMunitAssertions { a =>
      a.assertEquals(Day8.part1(input), BigInt(21))
    }
  }

  test("part2") {
    withMunitAssertions { a =>
      a.assertEquals(Day8.part2(input), BigInt("8"))
    }
  }
