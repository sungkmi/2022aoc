package dev.sungkm.adventofcode2022
package day1to10

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day4Test extends HedgehogSuite:

  val input = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""


  test("part1") {
    withMunitAssertions { assertions =>

      assertions.assertEquals(Day4.part1(input), BigInt(2))
    }
  }

  test("part2") {
    withMunitAssertions { assertions =>

      assertions.assertEquals(Day4.part2(input), BigInt(4))
    }
  }
