package dev.sungkm.adventofcode2022
package day1to10

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day1Test extends HedgehogSuite:

  val input = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

  test("part1") {
    withMunitAssertions { assertions =>

      val expected = BigInt(24000)

      assertions.assertEquals(Day1.part1(input), expected)
    }
  }

  test("part2") {
    withMunitAssertions { assertions =>

      val expected = BigInt(45000)

      assertions.assertEquals(Day1.part2(input), expected)
    }
  }
