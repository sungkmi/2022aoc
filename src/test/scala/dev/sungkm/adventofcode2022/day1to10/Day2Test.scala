package dev.sungkm.adventofcode2022
package day1to10

import hedgehog.munit.HedgehogSuite
import hedgehog.*

class Day2Test extends HedgehogSuite:

  val input = """A Y
B X
C Z
"""

  test("parseLine") {
    withMunitAssertions { assertions =>
      assertions.assertEquals(Day2.parseLine("A Y"), (1, 2))
    }
  }

  test("outcome") {
    withMunitAssertions { assertions =>
      assertions.assertEquals(Day2.outcome(1, 2), 6)
      assertions.assertEquals(Day2.outcome(2, 1), 0)
      assertions.assertEquals(Day2.outcome(3, 3), 3)
      assertions.assertEquals(Day2.outcome(3, 1), 6)
    }
  }

  test("part1") {
    withMunitAssertions { assertions =>

      assertions.assertEquals(Day2.part1(input), BigInt(15))
    }
  }

  test("part2") {
    withMunitAssertions { assertions =>

      assertions.assertEquals(Day2.part2(input), BigInt(12))
    }
  }
